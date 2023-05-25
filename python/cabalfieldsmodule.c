#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "cabalfields.h"


/********************************************************************
 * Exceptions
 ********************************************************************/

static PyObject *TokensError;

/********************************************************************
 * Tokens class
 ********************************************************************/

typedef struct {
    PyObject_HEAD
    Py_buffer contents;
    cf_parser_state state;
} Tokens;

static void
Tokens_dealloc(Tokens *self)
{
    PyBuffer_Release(&(self->contents));
    Py_TYPE(self)->tp_free((PyObject *) self);
}

static int
Tokens_init(Tokens *self, PyObject *args, PyObject *kwds) {
    static char *kwlist[] = {"", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwds, "y*", kwlist, &(self->contents))) {
        return -1;
    }

    cf_reset(&(self->state), self->contents.buf, self->contents.len);

    return 0;
}

PyObject *Tokens_iter(PyObject *self) {
    Py_INCREF(self);
    return self;
}

PyObject *Tokens_iternext(Tokens *self) {
    cf_token token = {0};
    cf_token_type ty = cf_next(&(self->state), &token);

#ifdef CF_HAS_TK_SKIP
    while (ty == CF_TK_SKIP) { ty = cf_next(&(self->state), &token); }
#endif

    // debug
    // cf_print_token_ex(&(self->state), ty, &token);

    switch (ty) {
        case CF_TK_UNEXPECTED_CHAR:
        case CF_TK_INCONSISTENT_INDENTATION:
        case CF_TK_STACKOVERFLOW:
            PyErr_SetString(TokensError, cf_token_type_str(ty));
            return NULL;

        case CF_TK_EOF:
            PyErr_SetNone(PyExc_StopIteration);
            return NULL;

        case CF_TK_SECTION:
            return Py_BuildValue("{s:s,s:y#,s:(i,i),s:y#}",
                "type", cf_token_type_str(ty),
				"name", self->state.contents + token.section.name_bgn, token.section.name_end - token.section.name_bgn,
				"name_pos", token.section.name_loc.row, token.section.name_loc.col,
				"args", self->state.contents + token.section.args_bgn, token.section.args_end - token.section.args_bgn);

        case CF_TK_FIELD:
            return Py_BuildValue("{s:s,s:y#,s:(i,i),s:(i,i)}",
                "type", cf_token_type_str(ty),
				"name", self->state.contents + token.field.name_bgn, token.field.name_end - token.field.name_bgn,
				"name_pos", token.field.name_loc.row, token.field.name_loc.col,
				"colon_pos", token.field.colon_loc.row, token.field.colon_loc.col);

        case CF_TK_FIELDLINE:
			return Py_BuildValue("{s:s,s:y#,s:(i,i)}",
                "type", cf_token_type_str(ty),
				"line", self->state.contents + token.fieldline.line_bgn, token.fieldline.line_end - token.fieldline.line_bgn,
				"line_pos", token.fieldline.line_loc.row, token.fieldline.line_loc.col);

        case CF_TK_COMMENT:
			return Py_BuildValue("{s:s,s:y#,s:(i,i)}",
                "type", cf_token_type_str(ty),
				"comment", self->state.contents + token.comment.bgn, token.comment.end - token.comment.bgn,
				"comment_pos", token.comment.loc.row, token.comment.loc.col);

        /* these tokens don't have payload. */
        case CF_TK_FIELD_END:
        case CF_TK_SECTION_END:
            return Py_BuildValue("{s:s}",
                "type", cf_token_type_str(ty));

        default:
            return Py_BuildValue("{s:s}",
                "type", cf_token_type_str(ty));
    }
}

// https://docs.python.org/3/extending/newtypes_tutorial.html
// https://docs.python.org/3/c-api/arg.html
// https://docs.python.org/3/extending/extending.html
// https://docs.python.org/3/c-api/dict.htm
static PyTypeObject Tokens_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "cabalfields.Tokens",
    .tp_doc       = PyDoc_STR("Cabal fields token stream"),
    .tp_basicsize = sizeof(Tokens),
    .tp_new       = PyType_GenericNew,
    .tp_itemsize  = 0,
    .tp_flags     = Py_TPFLAGS_DEFAULT,
    .tp_init      = (initproc)Tokens_init,
    .tp_dealloc   = (destructor)Tokens_dealloc,
    .tp_iter      = (getiterfunc)Tokens_iter,
    .tp_iternext  = (iternextfunc)Tokens_iternext,
};

/********************************************************************
 * cabalfields module
 ********************************************************************/

static PyModuleDef cabalfields_module = {
    PyModuleDef_HEAD_INIT,
    .m_name = "cabalfields",
    .m_doc = "Cabal fields tokenizer",
    .m_size = -1,
};

PyMODINIT_FUNC PyInit_cabalfields(void) {
    if (PyType_Ready(&Tokens_Type) < 0) { return NULL; }

    PyObject *m = PyModule_Create(&cabalfields_module);
    if (m == NULL) { return NULL; }

    TokensError = PyErr_NewException("cabalfields.TokensError", NULL, NULL);
    Py_XINCREF(TokensError);
    if (PyModule_AddObject(m, "TokensError", TokensError) < 0) {
        Py_XDECREF(TokensError);
        Py_CLEAR(TokensError);
        Py_DECREF(m);
        return NULL;
    }

    Py_INCREF(&Tokens_Type);
    if (PyModule_AddObject(m, "Tokens", (PyObject *) &Tokens_Type) < 0) {
        Py_DECREF(&Tokens_Type);
        Py_DECREF(m);
        return NULL;
    }

    return m;
}
