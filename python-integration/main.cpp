#define PY_SSIZE_T_CLEAN
#include <iostream>
#include <cstdlib>
#include <stdlib.h>
#include <string.h>

#include <Python.h>

#include "vartab.h"
#include "py_utils.h"

var_table* makeVartable() {
    var_table* vt = new var_table;
    vt->assign("string", var_data("string"));
    vt->assign("int", var_data(1));
    double arr[2] = {1, 2};
    vt->assign("array", var_data(arr, 2));
    return vt;
}

bool compareVartables(var_table* vt1, var_table* vt2) {
    if (!vt1 || !vt2)
        return false;
    for (size_t i = 0; i < vt1->size(); i++){
        const char* key = vt1->key(i);
        var_data* vd1 = vt1->lookup(key);
        int type = vd1->type;
        var_data* vd2 = vt2->lookup(key);
        if (!vd2)
            return false;
        switch (type){
            case SSC_STRING:
                if (strcmp(vd1->str.c_str(), vd2->str.c_str()) != 0){
                    printf("string %s vs %s\n", vd1->str.c_str(), vd2->str.c_str());
                    return false;
                }
                break;
            case SSC_NUMBER:
                if (vd1->num[0] != vd2->num[0]){
                    printf("number %f vs %f\n", vd1->num[0], vd2->num[0]);
                    return false;
                }
                break;
            case SSC_ARRAY:
                for (size_t n = 0; n < vd1->num.length(); n++){
                    if (vd1->num[n] != vd2->num[n]){
                        printf("array %f vs %f\n", vd1->num[n], vd2->num[n]);
                        return false;
                    }
                }
            default:
                break;
        }
    }
    return true;
}

int main(int argc, char *argv[]){

    var_table* vtReturned = nullptr;
    var_table* vt = makeVartable();


	// environment variable for python interpreter does not need to persist beyond program execution
	// path must be changed from SAM development directory (ENV(SAMNTDIR)) to SAM installation (via SamApp)
    std::string samdir = "PYTHONHOME=" + std::string(std::getenv("SAMNTDIR")) + "/deploy/python-3.7.4";
    putenv(const_cast<char*>(samdir.c_str()));
    printf("%s", samdir.c_str());
    const char scriptName[256] = "printReturn";
    const char funcName[16] = "printReturn";


    PyObject *pName, *pModule, *pFunc;
    PyObject *pArgs, *pValue;

    Py_Initialize();
    PyObject* python_dictionary = SAMPy_table_to_dict(vt);
    pName = PyUnicode_DecodeFSDefault(scriptName);
    /* Error checking of pName left out */

    pModule = PyImport_Import(pName);
    Py_DECREF(pName);

    if (pModule != NULL) {
        pFunc = PyObject_GetAttrString(pModule, funcName);
        /* pFunc is a new reference */

        if (pFunc && PyCallable_Check(pFunc)) {
            pArgs = PyTuple_New(1);
            PyTuple_SetItem(pArgs, 0, python_dictionary);
            pValue = PyObject_CallObject(pFunc, pArgs);
            Py_DECREF(pArgs);
            if (pValue != NULL) {
                vtReturned = SAMPy_dict_to_table(pValue);
                Py_DECREF(pValue);
            }
            else {
                Py_DECREF(pFunc);
                Py_DECREF(pModule);
                PyErr_Print();
                fprintf(stderr,"Call failed\n");
                return 1;
            }
        }
        else {
            if (PyErr_Occurred())
                PyErr_Print();
            fprintf(stderr, "Cannot find function \"%s\"\n", funcName);
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }
    else {
        PyErr_Print();
        fprintf(stderr, "Failed to load \"%s\"\n", scriptName);
        return 1;
    }
    if (Py_FinalizeEx() < 0) {
        return 120;
    }

    printf("Original == Returned? %d\n", (int)(compareVartables(vt, vtReturned)));

    delete vt;
    delete vtReturned;
    return 0;
}

