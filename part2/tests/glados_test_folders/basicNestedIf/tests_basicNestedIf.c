/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicNestedIf/tests_basicNestedIf.c
*/

#include "files_print.h"

extern int test(int a);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(test(1), path);
    write_int_output(test(11), path);
    write_int_output(test(111), path);
    write_int_output(test(1111), path);
    return (0);
}
