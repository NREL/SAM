#pragma once
#define system_advisor_model_EXPORTS
#if defined(_WIN32) || defined(__CYGWIN__)
#ifdef system_advisor_model_EXPORTS
    #ifdef __GNUC__
      #define SAM_EXPORT __attribute__ ((dllexport))
    #else
      #define SAM_EXPORT __declspec(dllexport)
    #endif
  #else
    #ifdef __GNUC__
      #define SAM_EXPORT __attribute__ ((dllimport))
    #else
      #define SAM_EXPORT __declspec(dllimport)
    #endif
  #endif
#else
#if __GNUC__ >= 4
#define SAM_EXPORT __attribute__ ((visibility ("default")))
#else
#define SAM_EXPORT
#endif
#endif