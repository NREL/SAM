classdef Info < handle
    %Info object from SSC
    %   Object used to gather ssc data meta information  name, label,
    %   units, constraints and other  information.
    
    properties (SetAccess = private)
        m_idx
        m_info
        m_module
    end
    properties
        Name
        VariableType
        DataType
        Label
        Units
        Meta
        Group
        Required
        Constraints
    end
    
    methods
        function obj = Info(module)
            obj.m_idx = 0;
            obj.m_module = module;
        end
        function Reset(obj)
            obj.m_idx = 0;
        end
        function result = Get(obj)
            obj.m_info = SSC.ssccall('module_var_info', obj.m_module.GetHandle(), obj.m_idx);
            if (obj.m_info == 0),
                obj.Reset;
                result = false;
            else
                obj.Name = SSC.ssccall('info_name', obj.m_info);
                obj.VariableType = SSC.ssccall('info_var_type', obj.m_info);
                obj.DataType = SSC.ssccall('info_data_type', obj.m_info);
                obj.Label = SSC.ssccall('info_label', obj.m_info);
                obj.Units = SSC.ssccall('info_units', obj.m_info);
                obj.Meta = SSC.ssccall('info_meta', obj.m_info);
                obj.Group = SSC.ssccall('info_group', obj.m_info);
                obj.Required = SSC.ssccall('info_required', obj.m_info);
                obj.Constraints = SSC.ssccall('info_constraints', obj.m_info);
                obj.m_idx = obj.m_idx + 1;
                result = true;
            end
        end
    end
    
end

