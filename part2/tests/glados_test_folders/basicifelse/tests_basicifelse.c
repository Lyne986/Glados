/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicifelse/tests_basicifelse.c
*/

#include "files_print.h"

int isEven(int a);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(isEven(0), path);
    write_int_output(isEven(1), path);
    return (0);
}
