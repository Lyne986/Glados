/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** get_test_path
*/

#include "files_print.h"

/* Using __FILE__ macro giving me the full path of the current .C file*/
char *get_test_file_path(char *full_path)
{
    char *path = malloc(sizeof(char) * strlen(full_path) + 1);

    memset(path, 0, strlen(full_path));
    for (int i = strlen(full_path); i > 0; i--) {

        if (full_path[i] == '/') {
            path = strncpy(path, full_path, i);
            break;
        }

    }
    return (path);
}