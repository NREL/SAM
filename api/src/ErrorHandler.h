/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

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
static bool translateExceptions(SAM_error* out_error, Fn&& fn)
{
    try {
        fn();
    } catch (const std::runtime_error& e) {
        *out_error = new error{e.what()};
        return false;
    } catch (const std::exception& e) {
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

    ~Error()
    {
        if (opaque) {
            error_destruct(opaque);
        }
    }

    SAM_error opaque;
};

class ThrowOnError {
public:
    ~ThrowOnError() noexcept(false)
    {
        if (_error.opaque) {
            throw std::runtime_error(error_message(_error.opaque));
        }
    }

    operator SAM_error*() { return &_error.opaque; }

private:
    Error _error;
};

