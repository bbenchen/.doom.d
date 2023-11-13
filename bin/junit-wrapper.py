#!/usr/bin/env python3

import argparse
import os
import requests
import subprocess

junit_platform_console_file = os.path.expanduser("~/.junit-wrapper/junit-platform-console-standalone.jar")

def download_junit_platform_console():
    junit_platform_console_download_url = "https://mirrors.huaweicloud.com/repository/maven/org/junit/platform/junit-platform-console-standalone/1.10.1/junit-platform-console-standalone-1.10.1.jar"
    with requests.get(junit_platform_console_download_url, allow_redirects=True) as request:
        open(junit_platform_console_file, 'wb').write(request.content)

def check_junit_platform_console_file():
    if (not os.path.exists(junit_platform_console_file)):
        junit_platform_console_dir = os.path.dirname(junit_platform_console_file)
        if (not os.path.exists(junit_platform_console_dir)):
            os.makedirs(junit_platform_console_dir)

        download_junit_platform_console()

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("classpaths_file",
                        help="类路径信息")
    parser.add_argument("test_class_or_method",
                        help="要测试的类或方法")

    known_args = parser.parse_args()

    check_junit_platform_console_file()

    classpaths = ""
    test_class_or_method = known_args.test_class_or_method
    test_mode = "-m" if test_class_or_method.rfind("#") > 0 else "-c"
    with open(known_args.classpaths_file) as classpaths_file:
        classpaths = classpaths_file.readline()

    os.remove(known_args.classpaths_file)

    subprocess.run(["java",
                     "-jar",
                     junit_platform_console_file,
                     "execute",
                     "--disable-banner",
                     "-cp",
                     classpaths,
                     test_mode,
                     test_class_or_method])

if __name__ == '__main__':
    main()
