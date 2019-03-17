#pragma once

#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>
#include <string>

#include "visibility.h"
#include "SAM_api.h"

extern "C" {


struct error {
    std::string message;
};

const char* error_message(SAM_error error);

void error_destruct(SAM_error error);

}

/// Returns true if fn executed without throwing an error, false otherwise.
/// If calling fn threw an error, capture it in *out_error.
template<typename Fn>
static bool translateExceptions(SAM_error* out_error, Fn&& fn)
{
    try {
        fn();
    } catch (const std::exception& e) {
        *out_error = new error{e.what()};
        return false;
    } catch (...) {
        *out_error = new error{"Unknown internal error"};
        return false;
    }
    return true;
}

namespace std {
    template<class T> struct _Unique_if {
        typedef unique_ptr<T> _Single_object;
    };

    template<class T> struct _Unique_if<T[]> {
        typedef unique_ptr<T[]> _Unknown_bound;
    };

    template<class T, size_t N> struct _Unique_if<T[N]> {
        typedef void _Known_bound;
    };

    template<class T, class... Args>
    typename _Unique_if<T>::_Single_object
    make_unique(Args&&... args) {
        return unique_ptr<T>(new T(std::forward<Args>(args)...));
    }

    template<class T>
    typename _Unique_if<T>::_Unknown_bound
    make_unique(size_t n) {
        typedef typename remove_extent<T>::type U;
        return unique_ptr<T>(new U[n]());
    }

    template<class T, class... Args>
    typename _Unique_if<T>::_Known_bound
    make_unique(Args&&...) = delete;
}
