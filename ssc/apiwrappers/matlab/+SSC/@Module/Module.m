classdef Module < handle
    %Module object from SSC
    %   Object used to manipulate ssc compute modules.
    
    properties (SetAccess = private)
        m_module
    end
    
    methods
        function obj = Module(name)
            obj.m_module = SSC.ssccall('module_create', name);
        end
        function delete(obj)
            if (obj.m_module ~= 0),
                SSC.ssccall('module_free', obj.m_module);
            end
         end
        function result = IsOk(obj)
            result = ( obj.m_module ~= 0 );
        end
        function result = Exec(obj, data )
            result = (SSC.ssccall('module_exec', obj.m_module, data.GetHandle()) ~= 0);
        end
        function [result, msg, type, time] = Log(obj, idx)
            msg = '';
            type = '';
            time = 0;
            logMessage = SSC.ssccall('module_log_detailed',obj.m_module, idx);
            [nrows ncols] = size(logMessage);
            if ( (nrows * ncols) > 1),
                msg = logMessage{1};
                type = logMessage{2};
                time = logMessage{3};
                result = true;
            else
                result = false;
            end
        end
        function result = GetHandle(obj)
            result = obj.m_module;
        end
    end
end

