import os
import sys

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

class launch_tests:
    test_dir = ""
    return_val = 0;

    def __init__(self, test_dir):

        help = "\nUsage: \n          -   python3 glados_tester.py -b <test_dir> \n          -   python3 glados_tester.py -l <test_dir>\n"
        if (not os.path.isdir(test_dir)):
            print(help)
            sys.exit(84)

        self.test_dir = test_dir
        self.test_dir = test_dir
        self.test_dir = self.test_dir.replace('/', '')

    def exec_test(self, dir, incldue_headers, tool_funcs):
        # compile .c
        object_name = self.test_dir + "/" + dir + "/test_" + dir + ".o"
        print(object_name)
        if (not os.path.isfile(object_name)):
            print("\n " + bcolors.WARNING + "[ " + bcolors.HEADER + dir + bcolors.WARNING + " ] <=======> END OF TEST  OBJECT FILE FOR " + dir + " DOESN'T EXIST\n\n" + bcolors.ENDC)
            return 1
        tool_funcs_str = ""
        test_file_name = self.test_dir + "/" + dir + "/tests_" + dir + ".c"
        include_path = self.test_dir + "/include"
        binary_path = self.test_dir + "/" + dir + "/" + dir + ".out"
        # print("TEST FILE NAME is ->", test_file_name)
        # print("OBJECT NAME ->", object_name)

        for i in tool_funcs:
            tool_funcs_str += " " + self.test_dir + "/tool_funcs/" + i
        # print(tool_funcs_str)
        # print(include_path)
        # print("binary path", binary_path)

        if (incldue_headers != []):
            os.system("gcc " + tool_funcs_str + " " + test_file_name + " " + object_name + " -I./" + include_path + " -o " + binary_path)
        else:
            os.system("gcc " + tool_funcs_str + " " + test_file_name + " -o " + binary_path)

        # run the binary
        # print("./" + binary_path)
        if (not os.path.isfile(binary_path)):
            print("\n " + bcolors.WARNING + "[ " + bcolors.HEADER + dir + bcolors.WARNING + " ] <=======> END OF TEST  BINARY FILE FOR " + dir + " DOESN'T EXIST\n\n" + bcolors.ENDC)
            return 1
        ret = os.system("./" + binary_path)
        # print("ret is ->", ret)
        # check if the output file exist
        if (not os.path.isfile(self.test_dir + "/" + dir + "/output")):
            print("\n " + bcolors.WARNING + "[ " + bcolors.HEADER + dir + bcolors.WARNING + " ] <=======> END OF TEST  OUTPUT FILE FOR " + dir + " DOESN'T EXIST\n\n" + bcolors.ENDC)
            return 1
        # compare the output with the expected output
        diff_return = os.system("diff " + self.test_dir + "/" + dir + "/expected_output " + self.test_dir + "/" + dir + "/output >" + self.test_dir + "/" + dir + "/diff_output")

        if (diff_return == 0):
            print("\n " + bcolors.OKGREEN + "[ " + bcolors.HEADER + dir + bcolors.OKGREEN + " ] <=======> END OF TEST  TEST PASSED\n\n" + bcolors.ENDC)
            return 0
        else:
            print("\n " + bcolors.FAIL + "[ " + bcolors.HEADER + dir + bcolors.FAIL + " ] <=======> END OF TEST  TEST FAILED CHECK -> " + self.test_dir + "/" + dir + "/diff_output" "\n\n" + bcolors.ENDC)
            return 1

    def get_incldue_headers(self):
        #check if include dir exists
        if (not os.path.isdir(self.test_dir + "/include")):
            return []
        # get all the headers in the include folder
        include_headers = []

        for file in os.listdir(self.test_dir + "/include"):
            if (file.endswith(".h")):
                include_headers.append(file)
        return include_headers

    def get_tool_files(self):
        #check if test_funcs dir exists
        if (not os.path.isdir(self.test_dir + "/tool_funcs")):
            return []
        # get all the headers in the include folder
        tool_funcs = []

        for file in os.listdir(self.test_dir + "/tool_funcs"):
            if (file.endswith(".c")):
                tool_funcs.append(file)
        return tool_funcs

    def build_object_files(self, test_dir):
        cl_files_str = ""

        for root, dirs, files in os.walk(self.test_dir):
            for dir in dirs:
                if (dir != "include" and dir != "tool_funcs"):
                    for file in os.listdir(self.test_dir + "/" + dir):
                        if (file.endswith(".cl")):
                            cl_files_str += " " + self.test_dir + "/" + dir + "/" + file
        if (os.system("./glados " + cl_files_str) != 0):
            print("Error while building the object files")
            sys.exit(1)

    def launch(self):
        incldue_headers = self.get_incldue_headers()
        tool_funcs = self.get_tool_files()

        self.build_object_files(self.test_dir)
        # check all files in the test_dir subfolders included
        for root, dirs, files in os.walk(self.test_dir):
            for dir in dirs:
                # check if the file is a test file
                if (dir != "include" and dir != "tool_funcs"):
                    # compile .c inside the dir
                    self.return_val = 0 if (self.exec_test(dir, incldue_headers, tool_funcs) == 0 and self.return_val == 0) else 1

        for root, dirs, files in os.walk(self.test_dir):
            for dir in dirs:
                if (dir != "include" and dir != "tool_funcs"):
                    if (os.path.isfile(self.test_dir + "/" + dir + "/" + dir + ".out")):
                        os.system("rm " + self.test_dir + "/" + dir + "/" + dir + ".out")
                    if (os.path.isfile(self.test_dir + "/" + dir + "/output")):
                        os.system("rm " + self.test_dir + "/" + dir + "/output")
                    if (os.path.isfile(self.test_dir + "/" + dir + "/" + dir + ".o")):
                        os.system("rm " + self.test_dir + "/" + dir + "/test_" + dir + ".o")
        return (self.return_val)
