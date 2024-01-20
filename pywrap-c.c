
#include <Python.h>

const char *split_statements(const char *orig, char **expr, PyObject **result) {

    //printf("%p", *expr);
    PyObject *err = NULL;
    // fucking c
    PyObject *pysrc = NULL;
    char *r0 = NULL, *r1 = NULL;
    PyObject *moduleMainString = NULL;
    PyObject *moduleMain = NULL;
    PyObject *func = NULL;
    PyObject *args = NULL;
    
    
    pysrc = PyUnicode_FromString(orig);
    err = PyErr_Occurred(); if (err != NULL) goto cleanup;
    
    moduleMainString = PyUnicode_FromString("__main__");
    err = PyErr_Occurred(); if (err != NULL) goto cleanup;
    moduleMain = PyImport_Import(moduleMainString);
    err = PyErr_Occurred(); if (err != NULL) goto cleanup;
    func = PyObject_GetAttrString(moduleMain, "split_statements");
    err = PyErr_Occurred(); if (err != NULL) goto cleanup;
    args = PyTuple_Pack(1, pysrc);
    err = PyErr_Occurred(); if (err != NULL) goto cleanup;
    *result = PyObject_CallObject(func, args);
    err = PyErr_Occurred(); if (err != NULL) goto cleanup;

    // according to the python docs, don't need to free
    // r0 and r1
    // python docs, as poor as ever, not really clear who owns
    // these resources, but I think it's result
    // so have to read the *r0 and *r1 before releasing the result
    // so return the result as an outparam, and release it in
    // haskell at the right time
    PyArg_ParseTuple(*result, "sz", &r0, &r1);
    err = PyErr_Occurred(); if (err != NULL) goto cleanup;

    *expr = r1;
cleanup:
    if (pysrc != NULL) {
        Py_DECREF(pysrc);
    }
    if (moduleMainString != NULL) {
        Py_DECREF(moduleMainString);
    }
    if (moduleMain != NULL) {
        Py_DECREF(moduleMain);
    }
    if (func != NULL) {
        Py_DECREF(func);
    }
    if (args != NULL) {
        Py_DECREF(args);
    }
    // has to be held on to until the char*s are read in haskell
    //if (result != NULL) {
    //    Py_DECREF(result);
    //}
    return r0;

}

// can't fail?
PyObject* cPy_None() {
    Py_RETURN_NONE;
}

void cPy_XINCREF(PyObject *o) {
    Py_XINCREF(o);
}

void cPy_XDECREF(PyObject *o) {
    Py_XDECREF(o);
}

void cPy_DECREF(PyObject *o) {
    Py_DECREF(o);
}

void cPy_INCREF(PyObject *o) {
    Py_INCREF(o);
}

int is_py_true(PyObject *o) {
    return o == Py_True;
}

int is_py_false(PyObject *o) {
    return o == Py_False;
}

PyObject *cPy_type(PyObject *o) {
    // is this ok?
    return (PyObject*) Py_TYPE(o);
}

PyObject *cPy_false(){
    Py_RETURN_FALSE;
}

PyObject *cPy_true(){
    Py_RETURN_TRUE;
}


