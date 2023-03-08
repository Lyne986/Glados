/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicfuncCall/tests_basicfuncCall.c
*/

#include "files_print.h"

extern int sum(int a, int b, int c, int d);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");

    write_int_output(sum(10, 20, 30, 40), path);
    return (0);
}
