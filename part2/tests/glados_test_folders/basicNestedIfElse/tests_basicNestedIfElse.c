/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicNestedIfElse/tests_basicNestedIfElse.c
*/

#include "files_print.h"

extern int test(int a);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");

    write_int_output(test(100), path);
    write_int_output(test(10), path);
    write_int_output(test(1), path);
    write_int_output(test(6), path);
    return (0);
}
