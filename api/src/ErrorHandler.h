#pragma once

#include <cstddef>
#include <memory>
#include <type_traits>
#include <stdexcept>
#include <utility>
#include <string>

#include "visibility.h"
#include "SAM_api.h"

extern "C" {


struct error {
    std::string message;
};

}

/// Returns true if fn executed without throwing an error, false otherwise.
/// If calling fn threw an error, capture it in *out_error.
template<typename Fn>
static bool translateExceptions(SAM_error *out_error, Fn &&fn) {
    try {
        fn();
    } catch (const std::runtime_error &e) {
        *out_error = new error{e.what()};
        return false;
    } catch (const std::exception &e) {
        *out_error = new error{e.what()};
        return false;
    } catch (...) {
        *out_error = new error{"Unknown internal error"};
        return false;
    }
    return true;
}

void make_access_error(const std::string &obj_name, const std::string &var);

//
// Error handling
//

struct Error {
    Error() : opaque(nullptr) {}

    ~Error() {
        if (opaque) {
            error_destruct(opaque);
        }
    }

    SAM_error opaque;
};

class ThrowOnError {
public:
    ~ThrowOnError() noexcept(false) {
        if (_error.opaque) {
            throw std::runtime_error(error_message(_error.opaque));
        }
    }

    operator SAM_error *() { return &_error.opaque; }

private:
    Error _error;
};

