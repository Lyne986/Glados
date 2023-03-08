import os
import sys

class build_tests:
    test_dir = ""

    def __init__(self, test_dir):
        help = "\nUsage: \n          -   python3 glados_tester.py -b <test_dir> \n          -   python3 glados_tester.py -l <test_dir>\n"
        if (not os.path.isdir(test_dir)):
            print(help)
            sys.exit(84)

        self.test_dir = test_dir
        self.test_dir = self.test_dir.replace('/', '')
        print("TEST DIR IS -> \n", self.test_dir)


    def write_c_file(self, c_fileObject):
        #write header with test name
        c_fileObject.write("/*\n\
** EPITECH PROJECT, 2023\n\
** B-FUN-500-MPL-5-2-glados-bastien.boymond\n\
** File description:\n\
** " + c_fileObject.name + "\n\
*/\n\
")

        # write main in the file
        c_fileObject.write('\n#include "files_print.h"\n\n')
        c_fileObject.write('int main(int argc, char **argv)\n{\n    char *path = strcat(get_test_file_path(__FILE__), "/output");\n    return (0);\n}\n')
        c_fileObject.close()

    def build_tests(self):
        #move all the test files in their respective folder in order to create to create tests_*.c and expected output
        # check all the files in the test_dir
        for file in os.listdir(self.test_dir):
            # check if the file is a test file
            if (file.startswith("test_") and file.endswith(".cl")):
                # get the name of the test
                test_name = file[5:]
                # get the name of the folder
                folder_name = test_name.split(".")[0]
                # check if the folder exist
                print("FOLDER NAME IS -> \n", test_name)
                print("CREATING FOLDER -> \n", self.test_dir + "/" + folder_name)
                if (not os.path.isdir(self.test_dir + "/" + folder_name)):
                    # create the folder
                    os.mkdir(self.test_dir + "/" + folder_name)
                # move the test file in the folder
                os.rename(self.test_dir + "/" + file, self.test_dir + "/" + folder_name + "/" + file)

                # check if the expected output file exist
                if (not os.path.isfile(self.test_dir + "/" + folder_name + "/expected_output")):
                    # create the expected output file
                    open(self.test_dir + "/" + folder_name + "/expected_output", "w+")

                    # check if the .c file exist
                if (not os.path.isfile(self.test_dir + "/" + folder_name + "/tests_" + folder_name + ".c")):
                    # create the .c file
                    c_path = self.test_dir + "/" + folder_name + "/tests_" + folder_name + ".c"
                    c_fileObject = open(c_path, "w+")
                    self.write_c_file(c_fileObject)
            elif (os.path.isdir(self.test_dir + "/" + file)):
                folder_name = file
                print("FOLDER NAME IS -> \n", folder_name)
                print("dir -> \n", file)
                if (folder_name == "include" or folder_name == "tool_funcs"):
                    continue
                if (not os.path.isfile(self.test_dir + "/" + folder_name + "/expected_output")):
                    # create the expected output file
                    open(self.test_dir + "/" + folder_name + "/expected_output", "w+")

                    # check if the .c file exist
                if (not os.path.isfile(self.test_dir + "/" + folder_name + "/tests_" + folder_name + ".c")):
                    # create the .c file
                    print(os.path.isfile(self.test_dir + "/" + folder_name + "/test_" + folder_name + ".c"))
                    print(self.test_dir + "/" + folder_name + "/test_" + folder_name + ".c")
                    c_path = self.test_dir + "/" + folder_name + "/tests_" + folder_name + ".c"
                    c_fileObject = open(c_path, "w+")
                    self.write_c_file(c_fileObject)