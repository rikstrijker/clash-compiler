#!/usr/bin/env python3
import json
import shutil
import subprocess
import re
import os
import tempfile
import checksumdir

HERE = os.path.abspath(os.path.dirname(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "../../.."))
CACHE_DIR = os.path.expanduser(os.path.join(HERE, "src_cache"))
PLAN_JSON = os.path.join(ROOT, "dist-newstyle/cache/plan.json")
IS_PREINSTALLED_RE = re.compile(r"^.*-[0-9]+(\.[0-9]*)*$")

HACKAGE_BASE_URL = "https://hackage.haskell.org/package"
HACKAGE_ARCHIVE_URL = HACKAGE_BASE_URL + "/{pkg_name}-{pkg_version}/{pkg_name}-{pkg_version}.tar.gz"
HACKAGE_CABAL_URL = HACKAGE_BASE_URL + "/{pkg_name}-{pkg_version}/{pkg_name}.cabal"


def find_clash_ghc_lib(plan):
    for package in plan:
        if package["pkg-name"] == "clash-ghc" and package["component-name"] == "lib":
            return package
    raise ValueError("Couldn't find clash-ghc library")


def is_preinstalled(package_id):
    # If a given package_id is of the form: <name>_<version>, then it was
    # shipped with the installed GHC version, so we don't need to build it.
    return bool(IS_PREINSTALLED_RE.match(package_id))


def get_dep_ids(plan_by_id, package):
    """
    Gather all package ids that need to be built, given a top-level package
    (usually clash-ghc). Given list is in order, but will probably contain
    duplicates.
    """
    package_id = package["id"]
    if not is_preinstalled(package_id):
        components = package.get("components", {})
        for dep_id in components.get("lib", {}).get("depends", ()):
            yield from get_dep_ids(plan_by_id, plan_by_id[dep_id])
        for dep_id in package.get("depends", ()):
            yield from get_dep_ids(plan_by_id, plan_by_id[dep_id])
        yield package_id


def filter_deps(plan_by_id, dep_ids):
    """
    Filter duplicate dep ids by pkg-name/pkg-version. Makes sure to keep the
    order of deps.
    """
    seen = set()
    for dep_id in dep_ids:
        dep = plan_by_id[dep_id]
        key = (dep["pkg-name"], dep["pkg-version"])
        if key not in seen:
            yield dep_id
        seen.add(key)


def prepare_local_dep(package):
    """Copy locally availabe source code to cache directory."""
    pkg_name = package["pkg-name"]
    pkg_version = package["pkg-version"]
    dirhash = checksumdir.dirhash(package["pkg-src"]["path"])
    target_dirname = "{}-{}-{}".format(pkg_name, pkg_version, dirhash)
    target = os.path.join(CACHE_DIR, target_dirname)
    if os.path.exists(target): shutil.rmtree(target)
    shutil.copytree(package["pkg-src"]["path"], target)
    return target


def preprare_hackage_dep(package):
    """Fetch package from hackage and copy it to cache directory."""
    pkg_name = package["pkg-name"]
    pkg_version = package["pkg-version"]
    target_dirname = package["id"]
    target = os.path.join(CACHE_DIR, target_dirname)
    archive_name = "{pkg_name}-{pkg_version}.tar.gz".format(**locals())
    hackage_archive_url = HACKAGE_ARCHIVE_URL.format(**locals())
    hackage_cabal_url = HACKAGE_CABAL_URL.format(**locals())

    if os.path.exists(target):
        return target

    with tempfile.TemporaryDirectory() as tmpdir:
        archive_dir = os.path.join(tmpdir, "{pkg_name}-{pkg_version}".format(**locals()))
        subprocess.run(["wget", hackage_archive_url], cwd=tmpdir, check=True)
        subprocess.run(["tar", "xzf", archive_name], cwd=tmpdir, check=True)
        os.remove(os.path.join(archive_dir, "{pkg_name}.cabal".format(**locals())))
        subprocess.run(["wget", hackage_cabal_url], cwd=archive_dir, check=True)
        shutil.move(archive_dir, target)

    return target


def prepare_git_dep(package):
    """Clone git repository and copy it to cache directory"""
    # TODO: Account for 'subdir' directive
    tag = package["pkg-src"]["source-repo"]["tag"]
    pkg_name = package["pkg-name"]
    target_dirname = "{}-{}".format(pkg_name, tag)

    # Check for existing cache
    target_dir = os.path.join(CACHE_DIR, target_dirname)
    if os.path.exists(target_dir):
        return target_dir

    location = package["pkg-src"]["source-repo"]["location"]
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_target = os.path.join(tmpdir, target_dirname)
        subprocess.run(["git", "clone", location, tmp_target], check=True)
        subprocess.run(["git", "checkout", tag], check=True, cwd=tmp_target)
        shutil.move(tmp_target, target_dir)

    return target_dir


def prepare_dep(package):
    """Fetch a given package and copy it to cache directory"""
    src = package.get("pkg-src", {
        "type": "repo-tar",
        "repo": {
            "type": "secure-repo",
            "uri": "http://hackage.haskell.org/"
        }
    })

    if src["type"] == "repo-tar":
        return preprare_hackage_dep(package)
    elif src["type"] == "source-repo" and src["source-repo"]["type"] == "git":
        return prepare_git_dep(package)
    elif src["type"] == "local":
        return prepare_local_dep(package)
    else:
        raise ValueError(
            "Unrecognized type {!r} in {!r}".format(src["type"], package["id"])
        )


def prepare_all_deps():
    """Fetch source code for clash-ghc and its dependencies"""
    os.makedirs(CACHE_DIR, exist_ok=True)

    plan = json.load(open(PLAN_JSON))["install-plan"]
    plan_by_id = {package["id"]: package for package in plan}

    clash_ghc = find_clash_ghc_lib(plan)
    dep_ids = get_dep_ids(plan_by_id, clash_ghc)
    filtered_dep_ids = filter_deps(plan_by_id, dep_ids)

    for dep_id in filtered_dep_ids:
        src_dir = prepare_dep(plan_by_id[dep_id])
        yield (plan_by_id[dep_id], src_dir)
