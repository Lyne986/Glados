/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicif/tests_basicif.c
*/

#include "files_print.h"

int isTwo(int nb);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(isTwo(2), path);
    write_int_output(isTwo(1), path);
    return (0);
}
