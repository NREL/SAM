#ifndef PY_UTILS_H
#define PY_UTILS_H

#include <Python.h>
#include <marshal.h>
#include <cstdio>

#if defined(__WINDOWS__)
#define strcasecmp _stricmp
#endif


//
// Functions for converting between Python and C types
//

static int SAMPy_seq_to_array(PyObject *value, double **arr, int *seqlen){
    PyObject* seq;
    int i;

    seq = PySequence_Fast(value, "error converting tuple to array: argument must be iterable");
    if(!seq)
        return -1;

    *seqlen = (int)PySequence_Fast_GET_SIZE(seq);
    *arr = (double*)malloc(*seqlen*sizeof(double));
    if(!*arr) {
        Py_DECREF(seq);
        PyErr_NoMemory(  );
        return -2;
    }

    for(i=0; i < *seqlen; i++) {
        PyObject *fitem;
        PyObject *item = PySequence_Fast_GET_ITEM(seq, i);

        if(!item) {
            Py_DECREF(seq);
            free(*arr);
            fprintf(stderr, "error converting tuple to array: could not get item");
            return -3;
        }
        if(!PyNumber_Check(item)) {
            Py_DECREF(seq);
            free(*arr);
            fprintf(stderr, "error converting tuple to array: all items must be numbers");
            return -4;
        }
        fitem = PyNumber_Float(item);

        (*arr)[i] = PyFloat_AS_DOUBLE(fitem);
        Py_DECREF(fitem);
    }
    Py_DECREF(seq);
    return 0;
}

static int SAMPy_seq_to_matrix(PyObject *value, double **mat, int *nrows, int *ncols){
    PyObject* seq, *row;
    int i;

    seq = PySequence_Fast(value, "argument must be iterable");
    if(!seq)
        return -1;

    row = PySequence_Fast_GET_ITEM(seq, 0);

    *nrows = (int)PySequence_Fast_GET_SIZE(seq);
    *ncols = (int)PySequence_Fast_GET_SIZE(row);

    *mat = (double*)malloc((*nrows)*(*ncols)*sizeof(double));
    double *arr = NULL;
    int seqlen;

    if(!*mat) {
        Py_DECREF(seq);
        PyErr_NoMemory(  );
        return -2;
    }
    for(i=0; i < *nrows; i++) {
        row = PySequence_Fast_GET_ITEM(seq, i);
        if (PySequence_Fast_GET_SIZE(row) != *ncols){
            free(*mat);
            Py_DECREF(seq);
            fprintf(stderr, "Matrix must be rectangular.");
            return -6;
        }
        int res = SAMPy_seq_to_array(row, &arr, &seqlen);
        if ( res < 0){
            free(*mat);
            Py_DECREF(seq);
            fprintf(stderr, "Error converting nested tuple %d into row in matrix.", i);
            return res;
        }
        double* mat_pos = &((*mat)[*ncols * i]);
        memcpy(mat_pos, arr, (*ncols)*sizeof(double));

        free(arr);
    }
    Py_DECREF(seq);
    return 0;
}

static PyObject* SAMPy_table_to_dict(var_table* table){
    size_t size, s = 0, i = 0, j=0;

    const char* str;
    double num;
    const double* arr;
    size_t n, m;

    size = table->size();

    PyObject* entry_obj;
    PyObject* table_dict = PyDict_New();
    PyObject* seq, *seqq;
    var_table* data = NULL;
    for(s=0; s < size; s++) {
        const char* key = table->key(s);
        int type = (int)table->lookup(key)->type;

        switch(type){
            case SSC_STRING:
                str = table->as_string(key);
                entry_obj = PyUnicode_FromString(str);
                PyDict_SetItemString(table_dict, key, entry_obj);
                Py_DECREF(entry_obj);
                break;
            case SSC_NUMBER:
                num = table->as_double(key);
                entry_obj = PyLong_FromDouble((double)num);
                PyDict_SetItemString(table_dict, key, entry_obj);
                Py_DECREF(entry_obj);
                break;
            case SSC_ARRAY:
                arr = table->as_array(key, &n);
                printf("%d\n", n);
                seq = PyTuple_New(n);
                for(i=0; i < n; i++) {
                    PyTuple_SET_ITEM(seq, i, PyFloat_FromDouble(arr[i]));
                }
                PyDict_SetItemString(table_dict, key, seq);
                Py_DECREF(seq);
                break;
            case SSC_MATRIX:
                arr = table->as_matrix(key, &n, &m);
                seq = PyTuple_New(n);
                for(i=0; i < n; i++) {
                    seqq = PyTuple_New(m);
                    for(j=0; j < m; j++) {
                        PyTuple_SET_ITEM(seqq, j, PyFloat_FromDouble(arr[i * m + j]));
                    }
                    PyTuple_SET_ITEM(seq, i, seqq);
                }
                PyDict_SetItemString(table_dict, key, seq);
                Py_XDECREF(seq);
                break;
            case SSC_TABLE:
                data = &table->lookup(key)->table;
                seq = SAMPy_table_to_dict(data);
                PyDict_SetItemString(table_dict, key, seq);
                Py_DECREF(seq);
                break;
            case SSC_INVALID:
            default:
                fprintf(stderr, "Table contains entry with invalid type. Types must be number, string"
                                                   ", sequence, or dict.");
                goto fail;
        }
    }
    return table_dict;
    fail:
    Py_XDECREF(table_dict);
    return NULL;
}


static var_table* SAMPy_dict_to_table(PyObject* dict){
    PyObject* key, *value;
    Py_ssize_t pos = 0;

    var_table* table = new var_table;

    Py_INCREF(dict);
    PyObject* ascii_mystring, *first = NULL;

    double *mat = NULL;
    double *arr = NULL;
    var_table* data_tab = NULL;
    while (PyDict_Next(dict, &pos, &key, &value)){
        ascii_mystring = PyUnicode_AsASCIIString(key);
        char* name = PyBytes_AsString(ascii_mystring);

        // numeric
        if (PyNumber_Check(value)){
            double val = PyFloat_AsDouble(value);

            table->assign(name, var_data(val));
        }
        // string
        else if (PyUnicode_Check(value)){
            PyObject* ascii_val = PyUnicode_AsASCIIString(value);
            char* val = PyBytes_AsString(ascii_val);

            table->assign(name, var_data(val));
            Py_DECREF(ascii_val);
        }
        // sequences
        else if (PySequence_Check(value)){
            first = PySequence_GetItem(value, 0);
            if (!first){
                char str[256];
                fprintf(stderr, "Error assigning empty tuple to %s", name);
                goto fail;
            }

            // matrix
            if (PySequence_Check(first)){
                int nrows, ncols;
                if(SAMPy_seq_to_matrix(value, &mat, &nrows, &ncols) < 0){
                    goto fail;
                }
                table->assign(name, var_data(mat, nrows, ncols));
                free(mat);
            }
            // array
            else{
                int seqlen;
                if(SAMPy_seq_to_array(value, &arr, &seqlen) < 0)
                    goto fail;
                table->assign(name, var_data(arr, seqlen));
                free(arr);
            }
            Py_XDECREF(first);
        }
        else {
            data_tab = SAMPy_dict_to_table(value);

            if (!data_tab)
                return NULL;

            table->assign(name, var_data(*data_tab));

            delete data_tab;
            data_tab = NULL;
        }
        Py_DECREF(ascii_mystring);
    }
    Py_XDECREF(dict);

    return table;
    fail:
    if (data_tab) delete data_tab;
    Py_XDECREF(dict);
    Py_DECREF(ascii_mystring);
    Py_XDECREF(first);
    if (table) delete table;
    return NULL;
}

#endif