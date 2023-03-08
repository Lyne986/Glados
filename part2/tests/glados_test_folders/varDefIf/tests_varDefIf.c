/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/varDefIf/tests_varDefIf.c
*/

#include "files_print.h"

extern int test();

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(test(), path);
    return (0);
}
