
#define SWIGJAVA


/* inline attribute */
#ifndef SWIGINLINE
# if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#   define SWIGINLINE inline
# else
#   define SWIGINLINE
# endif
#endif

/* attribute recognised by some compilers to avoid 'unused' warnings */
#ifndef SWIGUNUSED
# if defined(__GNUC__)
#   if !(defined(__cplusplus)) || (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4))
#     define SWIGUNUSED __attribute__ ((__unused__)) 
#   else
#     define SWIGUNUSED
#   endif
# elif defined(__ICC)
#   define SWIGUNUSED __attribute__ ((__unused__)) 
# else
#   define SWIGUNUSED 
# endif
#endif

#ifndef SWIG_MSC_UNSUPPRESS_4505
# if defined(_MSC_VER)
#   pragma warning(disable : 4505) /* unreferenced local function has been removed */
# endif 
#endif

#ifndef SWIGUNUSEDPARM
# ifdef __cplusplus
#   define SWIGUNUSEDPARM(p)
# else
#   define SWIGUNUSEDPARM(p) p SWIGUNUSED 
# endif
#endif

/* internal SWIG method */
#ifndef SWIGINTERN
# define SWIGINTERN static SWIGUNUSED
#endif

/* internal inline SWIG method */
#ifndef SWIGINTERNINLINE
# define SWIGINTERNINLINE SWIGINTERN SWIGINLINE
#endif

/* exporting methods */
#if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#  ifndef GCC_HASCLASSVISIBILITY
#    define GCC_HASCLASSVISIBILITY
#  endif
#endif

#ifndef SWIGEXPORT
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   if defined(STATIC_LINKED)
#     define SWIGEXPORT
#   else
#     define SWIGEXPORT __declspec(dllexport)
#   endif
# else
#   if defined(__GNUC__) && defined(GCC_HASCLASSVISIBILITY)
#     define SWIGEXPORT __attribute__ ((visibility("default")))
#   else
#     define SWIGEXPORT
#   endif
# endif
#endif

/* calling conventions for Windows */
#ifndef SWIGSTDCALL
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   define SWIGSTDCALL __stdcall
# else
#   define SWIGSTDCALL
# endif 
#endif

/* Deal with Microsoft's attempt at deprecating C standard runtime functions */
#if !defined(SWIG_NO_CRT_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_CRT_SECURE_NO_DEPRECATE)
# define _CRT_SECURE_NO_DEPRECATE
#endif

/* Deal with Microsoft's attempt at deprecating methods in the standard C++ library */
#if !defined(SWIG_NO_SCL_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_SCL_SECURE_NO_DEPRECATE)
# define _SCL_SECURE_NO_DEPRECATE
#endif



/* Fix for jlong on some versions of gcc on Windows */
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
  typedef long long __int64;
#endif

/* Fix for jlong on 64-bit x86 Solaris */
#if defined(__x86_64)
# ifdef _LP64
#   undef _LP64
# endif
#endif

#include <jni.h>
#include <stdlib.h>
#include <string.h>


/* Support for throwing Java exceptions */
typedef enum {
  SWIG_JavaOutOfMemoryError = 1, 
  SWIG_JavaIOException, 
  SWIG_JavaRuntimeException, 
  SWIG_JavaIndexOutOfBoundsException,
  SWIG_JavaArithmeticException,
  SWIG_JavaIllegalArgumentException,
  SWIG_JavaNullPointerException,
  SWIG_JavaDirectorPureVirtual,
  SWIG_JavaUnknownError
} SWIG_JavaExceptionCodes;

typedef struct {
  SWIG_JavaExceptionCodes code;
  const char *java_exception;
} SWIG_JavaExceptions_t;


static void SWIGUNUSED SWIG_JavaThrowException(JNIEnv *jenv, SWIG_JavaExceptionCodes code, const char *msg) {
  jclass excep;
  static const SWIG_JavaExceptions_t java_exceptions[] = {
    { SWIG_JavaOutOfMemoryError, "java/lang/OutOfMemoryError" },
    { SWIG_JavaIOException, "java/io/IOException" },
    { SWIG_JavaRuntimeException, "java/lang/RuntimeException" },
    { SWIG_JavaIndexOutOfBoundsException, "java/lang/IndexOutOfBoundsException" },
    { SWIG_JavaArithmeticException, "java/lang/ArithmeticException" },
    { SWIG_JavaIllegalArgumentException, "java/lang/IllegalArgumentException" },
    { SWIG_JavaNullPointerException, "java/lang/NullPointerException" },
    { SWIG_JavaDirectorPureVirtual, "java/lang/RuntimeException" },
    { SWIG_JavaUnknownError,  "java/lang/UnknownError" },
    { (SWIG_JavaExceptionCodes)0,  "java/lang/UnknownError" }
  };
  const SWIG_JavaExceptions_t *except_ptr = java_exceptions;

  while (except_ptr->code != code && except_ptr->code)
    except_ptr++;

  (*jenv)->ExceptionClear(jenv);
  excep = (*jenv)->FindClass(jenv, except_ptr->java_exception);
  if (excep)
    (*jenv)->ThrowNew(jenv, excep, msg);
}


/* Contract support */

#define SWIG_contract_assert(nullreturn, expr, msg) if (!(expr)) {SWIG_JavaThrowException(jenv, SWIG_JavaIllegalArgumentException, msg); return nullreturn; } else


#include "sscapi.h"


#ifdef __cplusplus
extern "C" {
#endif

SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1version(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)ssc_version();
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1build_1info(JNIEnv *jenv, jclass jcls) {
  jstring jresult = 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  result = (char *)ssc_build_info();
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1INVALID_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(0);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1STRING_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(1);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1NUMBER_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(2);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1ARRAY_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(3);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1MATRIX_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(4);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1TABLE_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(5);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jlong JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1create(JNIEnv *jenv, jclass jcls) {
  jlong jresult = 0 ;
  ssc_data_t result;
  
  (void)jenv;
  (void)jcls;
  result = (ssc_data_t)ssc_data_create();
  *(ssc_data_t *)&jresult = result; 
  return jresult;
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1free(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  ssc_data_free(arg1);
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1clear(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  ssc_data_clear(arg1);
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1unassign(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *arg2 = (char *) 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  arg2 = 0;
  if (jarg2) {
    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);
    if (!arg2) return ;
  }
  ssc_data_unassign(arg1,(char const *)arg2);
  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1query(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {
  jint jresult = 0 ;
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *arg2 = (char *) 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  arg2 = 0;
  if (jarg2) {
    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);
    if (!arg2) return 0;
  }
  result = (int)ssc_data_query(arg1,(char const *)arg2);
  jresult = (jint)result; 
  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1first(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  result = (char *)ssc_data_first(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1next(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  result = (char *)ssc_data_next(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1set_1string(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2, jstring jarg3) {
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *arg2 = (char *) 0 ;
  char *arg3 = (char *) 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  arg2 = 0;
  if (jarg2) {
    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);
    if (!arg2) return ;
  }
  arg3 = 0;
  if (jarg3) {
    arg3 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg3, 0);
    if (!arg3) return ;
  }
  ssc_data_set_string(arg1,(char const *)arg2,(char const *)arg3);
  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);
  if (arg3) (*jenv)->ReleaseStringUTFChars(jenv, jarg3, (const char *)arg3);
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1set_1number(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jfloat value) 
{
  ssc_data_t ssc_cxt = (ssc_data_t) 0 ;
  char *ssc_name = (char *) 0 ;
  ssc_number_t ssc_value ;
  
  ssc_cxt = *(ssc_data_t *)&cxt; 
  ssc_name = 0;
  if (name) 
  {
    ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);
    if (!ssc_name) return ;
  }
  ssc_value = (ssc_number_t)value; 
  ssc_data_set_number(ssc_cxt,(char const *)ssc_name,ssc_value);
  if (ssc_name) (*jenv)->ReleaseStringUTFChars(jenv, name, (const char *)ssc_name);
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1set_1array(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jfloatArray value, jint len) 
{
	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;
	char *ssc_name = (char *) 0 ;
	ssc_number_t ssc_value ;
  
	ssc_cxt = *(ssc_data_t *)&cxt; 
	ssc_name = 0;
	if (name) 
	{
      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);
	  if (!ssc_name) return ;
	}
    
	int i, count;
    jfloat *j_data_array;
    jint j_count_len;

    j_count_len = (*jenv)->GetArrayLength(jenv, value);
    j_data_array = (*jenv)->GetFloatArrayElements(jenv, value, NULL);
    // set data
    count = j_count_len;
    ssc_number_t ssc_array[count];
    for( i=0; i<count;i++)
        ssc_array[i] = j_data_array[i];
    
    ssc_data_set_array( ssc_cxt, ssc_name, ssc_array, count);
    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);
    (*jenv)->ReleaseFloatArrayElements(jenv, value, j_data_array, 0);
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1set_1matrix(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jfloatArray value, jint nrow, jint ncol) 
{
	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;
	char *ssc_name = (char *) 0 ;
	ssc_number_t ssc_value ;
  
	ssc_cxt = *(ssc_data_t *)&cxt; 
	ssc_name = 0;
	if (name) 
	{
      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);
	  if (!ssc_name) return ;
	}

	int i, rows, cols;
    jfloat *j_data_array;
    jint j_count_len;

    j_count_len = (*jenv)->GetArrayLength(jenv, value);
    j_data_array = (*jenv)->GetFloatArrayElements(jenv, value, NULL);
    // set data
    ssc_number_t ssc_array[j_count_len];
    for( i=0; i<j_count_len;i++)
        ssc_array[i] = j_data_array[i];
    
    rows = nrow;
    cols = ncol;
    ssc_data_set_matrix( ssc_cxt, ssc_name, ssc_array, rows, cols);
    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);
    (*jenv)->ReleaseFloatArrayElements(jenv, value, j_data_array, 0);
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1set_1table(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2, jlong jarg3) {
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *arg2 = (char *) 0 ;
  ssc_data_t arg3 = (ssc_data_t) 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  arg2 = 0;
  if (jarg2) {
    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);
    if (!arg2) return ;
  }
  arg3 = *(ssc_data_t *)&jarg3; 
  ssc_data_set_table(arg1,(char const *)arg2,arg3);
  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1get_1string(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {
  jstring jresult = 0 ;
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *arg2 = (char *) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  arg2 = 0;
  if (jarg2) {
    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);
    if (!arg2) return 0;
  }
  result = (char *)ssc_data_get_string(arg1,(char const *)arg2);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1get_1number(JNIEnv *env, jclass jcls, jlong cxt, jstring name, jfloatArray value) 
{
  jint jresult = 0 ;
  ssc_data_t sscCxt = (ssc_data_t) 0 ;
  char *sscName = (char *) 0 ;
  ssc_bool_t result;
  ssc_number_t sscValue;
  jfloat *output;
  jint count;

  count = (*env)->GetArrayLength(env, value);
  output = (*env)->GetFloatArrayElements(env, value, NULL);
  
  sscCxt = *(ssc_data_t *)&cxt; 
  sscName = 0;
  if (name) 
  {
    sscName = (char *)(*env)->GetStringUTFChars(env, name, 0);
    if (!sscName)
    {
        return 0;
    }
  }
  result = (ssc_bool_t)ssc_data_get_number(sscCxt,(char const *)sscName,&sscValue);
  jresult = (jint)result; 
  if (sscName) 
  {
      (*env)->ReleaseStringUTFChars(env, name, (const char *)sscName);
  }
  if (result && (count==1))
  {
      output[0] = sscValue;
      (*env)->SetFloatArrayRegion( env, value, 0, count, output );
  }
  else
  {
      jresult = 0;
  }
  return jresult;
}


SWIGEXPORT jfloatArray JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1get_1array(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name) 
{
	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;
	char *ssc_name = (char *) 0 ;
  
	ssc_cxt = *(ssc_data_t *)&cxt; 
	ssc_name = 0;
	if (name) 
	{
      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);
	  if (!ssc_name) return ;
	}

	
	int i, count;
    jfloatArray output;
    jint j_data_len;
    jfloat *j_data_array;

    // retrieve data
    ssc_number_t *ssc_array = ssc_data_get_array( ssc_cxt, ssc_name, &count);
    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);

    j_data_len = count;
 
    // return double array data
    output = (*jenv)->NewFloatArray( jenv, j_data_len );
    if (output == NULL) return NULL;

    j_data_array = (*jenv)->GetFloatArrayElements(jenv, output, NULL);

    for( i=0; i<count;i++)
            j_data_array[i] = ssc_array[i];

    (*jenv)->SetFloatArrayRegion( jenv, output, 0, j_data_len, j_data_array );
    return output;
}
 

SWIGEXPORT jfloatArray JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1get_1matrix(JNIEnv *jenv, jclass jcls, jlong cxt, jstring name, jintArray len) 
{
	ssc_data_t ssc_cxt = (ssc_data_t) 0 ;
	char *ssc_name = (char *) 0 ;
  
	ssc_cxt = *(ssc_data_t *)&cxt; 
	ssc_name = 0;
	if (name) 
	{
      ssc_name = (char *)(*jenv)->GetStringUTFChars(jenv, name, 0);
	  if (!ssc_name) return ;
	}


    int i, row_count, col_count;
    jfloatArray output;
    jint j_row_len;
    jint j_col_len;
    jfloat *j_data_array;
    jint j_count_len;
    jsize *j_count_array;
    jint j_data_len;

    // retrieve data
    ssc_number_t *ssc_array = ssc_data_get_matrix( ssc_cxt, ssc_name, &row_count, &col_count);
    (*jenv)->ReleaseStringUTFChars(jenv, name, ssc_name);

    j_row_len=row_count;
    j_col_len=col_count;
    j_data_len = row_count*col_count;
    // return row_count of elements in len argument element 0
    // return col_count of elements in len argument element 1
    j_count_len = (*jenv)->GetArrayLength(jenv, len);
    j_count_array = (*jenv)->GetIntArrayElements(jenv, len, NULL);

    if ( j_count_len > 1)
    {
            j_count_array[0] = j_row_len;
            j_count_array[1] = j_col_len;
    }

    (*jenv)->SetIntArrayRegion( jenv, len, 0, j_count_len, j_count_array );

    // return double array data
    output = (*jenv)->NewFloatArray( jenv, j_data_len );
    if (output == NULL) return NULL;

    j_data_array = (*jenv)->GetFloatArrayElements(jenv, output, NULL);

    for( i=0; i<j_data_len;i++)
            j_data_array[i] = ssc_array[i];

    (*jenv)->SetFloatArrayRegion( jenv, output, 0, j_data_len, j_data_array );
    return output;
}


SWIGEXPORT jlong JNICALL Java_SSC_SSCAPIJNI_ssc_1data_1get_1table(JNIEnv *jenv, jclass jcls, jlong jarg1, jstring jarg2) {
  jlong jresult = 0 ;
  ssc_data_t arg1 = (ssc_data_t) 0 ;
  char *arg2 = (char *) 0 ;
  ssc_data_t result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_data_t *)&jarg1; 
  arg2 = 0;
  if (jarg2) {
    arg2 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg2, 0);
    if (!arg2) return 0;
  }
  result = (ssc_data_t)ssc_data_get_table(arg1,(char const *)arg2);
  *(ssc_data_t *)&jresult = result; 
  if (arg2) (*jenv)->ReleaseStringUTFChars(jenv, jarg2, (const char *)arg2);
  return jresult;
}


SWIGEXPORT jlong JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1entry(JNIEnv *jenv, jclass jcls, jint jarg1) {
  jlong jresult = 0 ;
  int arg1 ;
  ssc_entry_t result;
  
  (void)jenv;
  (void)jcls;
  arg1 = (int)jarg1; 
  result = (ssc_entry_t)ssc_module_entry(arg1);
  *(ssc_entry_t *)&jresult = result; 
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1entry_1name(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_entry_t arg1 = (ssc_entry_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_entry_t *)&jarg1; 
  result = (char *)ssc_entry_name(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1entry_1description(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_entry_t arg1 = (ssc_entry_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_entry_t *)&jarg1; 
  result = (char *)ssc_entry_description(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1entry_1version(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jint jresult = 0 ;
  ssc_entry_t arg1 = (ssc_entry_t) 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_entry_t *)&jarg1; 
  result = (int)ssc_entry_version(arg1);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jlong JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1create(JNIEnv *jenv, jclass jcls, jstring jarg1) {
  jlong jresult = 0 ;
  char *arg1 = (char *) 0 ;
  ssc_module_t result;
  
  (void)jenv;
  (void)jcls;
  arg1 = 0;
  if (jarg1) {
    arg1 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg1, 0);
    if (!arg1) return 0;
  }
  result = (ssc_module_t)ssc_module_create((char const *)arg1);
  *(ssc_module_t *)&jresult = result; 
  if (arg1) (*jenv)->ReleaseStringUTFChars(jenv, jarg1, (const char *)arg1);
  return jresult;
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1free(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  ssc_module_t arg1 = (ssc_module_t) 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_module_t *)&jarg1; 
  ssc_module_free(arg1);
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1INPUT_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(1);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1OUTPUT_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(2);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1INOUT_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(3);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jlong JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1var_1info(JNIEnv *jenv, jclass jcls, jlong jarg1, jint jarg2) {
  jlong jresult = 0 ;
  ssc_module_t arg1 = (ssc_module_t) 0 ;
  int arg2 ;
  ssc_info_t result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_module_t *)&jarg1; 
  arg2 = (int)jarg2; 
  result = (ssc_info_t)ssc_module_var_info(arg1,arg2);
  *(ssc_info_t *)&jresult = result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1var_1type(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jint jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (int)ssc_info_var_type(arg1);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1data_1type(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jint jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (int)ssc_info_data_type(arg1);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1name(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_name(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1label(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_label(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1units(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_units(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1meta(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_meta(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1group(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_group(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1required(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_required(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1constraints(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_constraints(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1info_1uihint(JNIEnv *jenv, jclass jcls, jlong jarg1) {
  jstring jresult = 0 ;
  ssc_info_t arg1 = (ssc_info_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_info_t *)&jarg1; 
  result = (char *)ssc_info_uihint(arg1);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1exec_1simple(JNIEnv *jenv, jclass jcls, jstring jarg1, jlong jarg2) {
  jint jresult = 0 ;
  char *arg1 = (char *) 0 ;
  ssc_data_t arg2 = (ssc_data_t) 0 ;
  ssc_bool_t result;
  
  (void)jenv;
  (void)jcls;
  arg1 = 0;
  if (jarg1) {
    arg1 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg1, 0);
    if (!arg1) return 0;
  }
  arg2 = *(ssc_data_t *)&jarg2; 
  result = (ssc_bool_t)ssc_module_exec_simple((char const *)arg1,arg2);
  jresult = (jint)result; 
  if (arg1) (*jenv)->ReleaseStringUTFChars(jenv, jarg1, (const char *)arg1);
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1exec_1simple_1nothread(JNIEnv *jenv, jclass jcls, jstring jarg1, jlong jarg2) {
  jstring jresult = 0 ;
  char *arg1 = (char *) 0 ;
  ssc_data_t arg2 = (ssc_data_t) 0 ;
  char *result = 0 ;
  
  (void)jenv;
  (void)jcls;
  arg1 = 0;
  if (jarg1) {
    arg1 = (char *)(*jenv)->GetStringUTFChars(jenv, jarg1, 0);
    if (!arg1) return 0;
  }
  arg2 = *(ssc_data_t *)&jarg2; 
  result = (char *)ssc_module_exec_simple_nothread((char const *)arg1,arg2);
  if (result) jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
  if (arg1) (*jenv)->ReleaseStringUTFChars(jenv, jarg1, (const char *)arg1);
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1LOG_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(0);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1UPDATE_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(1);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1EXECUTE_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(2);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1exec(JNIEnv *jenv, jclass jcls, jlong cxt_module, jlong cxt_data) {
  jint jresult = 0 ;
  ssc_module_t arg1 = (ssc_module_t) 0 ;
  ssc_data_t arg2 = (ssc_data_t) 0 ;
  ssc_bool_t result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_module_t *)&cxt_module; 
  arg2 = *(ssc_data_t *)&cxt_data; 
  result = (ssc_bool_t)ssc_module_exec(arg1,arg2);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1exec_1with_1handler(JNIEnv *jenv, jclass jcls, jlong jarg1, jlong jarg2, jlong jarg3, jlong jarg4) {
  jint jresult = 0 ;
  ssc_module_t arg1 = (ssc_module_t) 0 ;
  ssc_data_t arg2 = (ssc_data_t) 0 ;
  ssc_bool_t (*arg3)(ssc_module_t,ssc_handler_t,int,float,float,char const *,char const *,void *) = (ssc_bool_t (*)(ssc_module_t,ssc_handler_t,int,float,float,char const *,char const *,void *)) 0 ;
  void *arg4 = (void *) 0 ;
  ssc_bool_t result;
  
  (void)jenv;
  (void)jcls;
  arg1 = *(ssc_module_t *)&jarg1; 
  arg2 = *(ssc_data_t *)&jarg2; 
  arg3 = *(ssc_bool_t (**)(ssc_module_t,ssc_handler_t,int,float,float,char const *,char const *,void *))&jarg3; 
  arg4 = *(void **)&jarg4; 
  result = (ssc_bool_t)ssc_module_exec_with_handler(arg1,arg2,arg3,arg4);
  jresult = (jint)result; 
  return jresult;
}



SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1NOTICE_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(1);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1WARNING_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(2);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jint JNICALL Java_SSC_SSCAPIJNI_SSC_1ERROR_1get(JNIEnv *jenv, jclass jcls) {
  jint jresult = 0 ;
  int result;
  
  (void)jenv;
  (void)jcls;
  result = (int)(3);
  jresult = (jint)result; 
  return jresult;
}


SWIGEXPORT jstring JNICALL Java_SSC_SSCAPIJNI_ssc_1module_1log(JNIEnv *jenv, jclass jcls, jlong cxt, jint index, jintArray type, jfloatArray time) 
{
  ssc_data_t ssc_cxt = (ssc_data_t) 0 ;
  ssc_cxt = *(ssc_data_t *)&cxt; 

	
  jstring jresult = 0 ;
  char *result = 0 ;
  jint outTypeCount;
  jint *outType;
  jint outTimeCount;
  jfloat *outTime;

  outTypeCount = (*jenv)->GetArrayLength(jenv, type);
  outType = (*jenv)->GetIntArrayElements(jenv, type, NULL);
  outTimeCount = (*jenv)->GetArrayLength(jenv, time);
  outTime = (*jenv)->GetFloatArrayElements(jenv, time, NULL);
  
  int ssc_log_type;
  float ssc_time;
  result = (char *)ssc_module_log(ssc_cxt,index,&ssc_log_type,&ssc_time);
      
  if (result && (outTypeCount==1) && (outTimeCount==1))
  {
      jresult = (*jenv)->NewStringUTF(jenv, (const char *)result);
      outType[0] = ssc_log_type;
      (*jenv)->SetIntArrayRegion( jenv, type, 0, outTypeCount, outType );
      outTime[0] = ssc_time;
      (*jenv)->SetFloatArrayRegion( jenv, time, 0, outTimeCount, outTime );
  }
  else
  {
      jresult = 0;
  }
  return jresult;
}


SWIGEXPORT void JNICALL Java_SSC_SSCAPIJNI__1_1ssc_1segfault(JNIEnv *jenv, jclass jcls) {
  (void)jenv;
  (void)jcls;
  __ssc_segfault();
}


#ifdef __cplusplus
}
#endif

