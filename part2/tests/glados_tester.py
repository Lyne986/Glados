import os
import sys

import tester.build_tests as build_tests
import tester.launch_tests as launch_tests

def main():
    help = "\nUsage: \n          -   python3 glados_tester.py -b <test_dir> \n          -   python3 glados_tester.py -l <test_dir>\n"

    if (len(sys.argv) != 2 and len(sys.argv) != 3):
        print("\n Something went wrong usage : \n\n python3 glados_tester.py -h or --help for help\n")
        sys.exit(84)

    if len(sys.argv) == 2:
        if sys.argv[1] == "-h" or sys.argv[1] == "--help":
            print(help)
            sys.exit(1)
        else:
            print(help)
            sys.exit(84)

    if sys.argv[1] == "-b":
        return_val = build_tests.build_tests(sys.argv[2]).build_tests()
        sys.exit(return_val)
    if sys.argv[1] == "-l":
        return_val = launch_tests.launch_tests(sys.argv[2]).launch()
        sys.exit(return_val)

if __name__ == "__main__":
    main()

# glados launch all .cl files in the given filepath subfolders in order to have their .o
# must launch functions inside the .o with the .c of the respective test folder
# must compare the output file with the expected output file