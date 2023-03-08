/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/varSwitch/tests_varSwitch.c
*/

#include "files_print.h"

extern int varSwitch();

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(varSwitch(), path);
    return (0);
}
