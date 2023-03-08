/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/longIf/tests_longIf.c
*/

#include "files_print.h"

extern int isEven(long nb);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(isEven(42), path);
    return (0);
}
