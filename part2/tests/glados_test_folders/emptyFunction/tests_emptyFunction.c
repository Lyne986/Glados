/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/emptyFunction/tests_emptyFunction.c
*/

#include "files_print.h"

extern int func();

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    func();
    create_empty_file(path);
    return (0);
}
