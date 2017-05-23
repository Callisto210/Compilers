#!/usr/bin/env python
import sys
import filecmp
import unittest
import os

class AcceptanceTests(unittest.TestCase):

    @classmethod
    def add_test(cls, dirpath, filename):
        basename = os.path.splitext(filename)[0]

        if dirpath.startswith('./'):
            dirpath = dirpath[2:]
        if dirpath.endswith('/'):
            dirpath = dirpath[:-1]

        name = os.path.join(dirpath, basename)

        def file2func_name(filename):
            return 'test_' + filename

        def test_func(self):
            os.system("python main.py {2}.in > {2}.actual".format(dirpath,filename,name))
            file_actual = "{0}.actual".format(name)
            file_expected = "{0}.expected".format(name)
            res = filecmp.cmp(file_actual, file_expected)
            self.assertTrue(res, "files {0} and {1} differ\n---ACTUAL---\n{2}\n---EXPECTED---\n{3}\n---".format(file_actual,
                                                                                                                file_expected,
                                                                                                                open(file_actual, 'r').read(),
                                                                                                                open(file_expected, 'r').read()
                                                                                                               ))

        func_name = file2func_name(name)
        setattr(cls, func_name, test_func)

    @classmethod
    def add_tests(cls, dir):
        for dirpath, dirnames, filenames in os.walk(dir):
            for filename in filenames:
                if filename.startswith('.'):
                    continue
                elif filename.endswith('.in'):
                    cls.add_test(dirpath,filename)

if __name__ == '__main__':

    test_dir = sys.argv[1] if len(sys.argv) > 1 else "tests_err"
    sys.argv = [sys.argv[0]]
    AcceptanceTests.add_tests(test_dir)
    unittest.main()
