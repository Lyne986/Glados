/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicAdd3Function/tests_basicAdd3Function.c
*/

#include "files_print.h"

extern int add(int a, int b, int c);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");

    write_int_output(add(10, 20, 30), path);
    write_int_output(add(0, 0, 0), path);
    return (0);
}
