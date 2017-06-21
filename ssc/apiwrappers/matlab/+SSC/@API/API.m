classdef API < handle
    %API Meta data from SSC
    %   Version information and Build information from ssc dll
    
    properties (Constant)
        % constants for return value of Info.VarType() (see sscapi.h)
        INPUT = 1;
        OUTPUT = 2;
        INOUT = 3;
        % constants for out integer type in Module.Log() method (see sscapi.h)
        NOTICE = 1;
        WARNING = 2;
        ERROR = 3;
        % constants for return value of Data.Query() and Info.DataType() (see sscapi.h)
        INVALID = 0;
        STRING = 1;
        NUMBER = 2;
        ARRAY = 3;
        MATRIX = 4;
        TABLE = 5;
    end
    properties
        Version
        BuildInfo
    end
    
    methods
        function obj = API()
            obj.Version =  SSC.ssccall('version');
            obj.BuildInfo = SSC.ssccall('build_info');
        end
    end
    
end

