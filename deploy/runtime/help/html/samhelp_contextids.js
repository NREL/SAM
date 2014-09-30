var hmContextIds = new Array();
function hmGetContextId(query) {
    var urlParams;
    var match,
        pl = /\+/g,
        search = /([^&=]+)=?([^&]*)/g,
        decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
    params = {};
    while (match = search.exec(query))
       params[decode(match[1])] = decode(match[2]);
    if (params["contextid"]) return decodeURIComponent(hmContextIds[params["contextid"]]);
    else return "";
}

hmContextIds["41500"]="pv_module.htm";
hmContextIds["41600"]="pv_inverter.htm";
hmContextIds["41200"]="pv_system_design.htm";
hmContextIds["41400"]="pv_shading.htm";
hmContextIds["42000"]="parabolic_trough_empirical.htm";
hmContextIds["42200"]="troughempirical_solar_field.htm";
hmContextIds["42300"]="troughempirical_collectors_scas.htm";
hmContextIds["42400"]="troughempirical_power_block.htm";
hmContextIds["42500"]="troughempirical_thermal_storage.htm";
hmContextIds["42600"]="troughempirical_parasitics.htm";
hmContextIds["44000"]="power_tower_molten_salt.htm";
hmContextIds["44300"]="towerms_heliostat_field.htm";
hmContextIds["44400"]="towerms_tower_and_receiver.htm";
hmContextIds["44500"]="towerms_power_cycle.htm";
hmContextIds["44600"]="towerms_thermal_storage.htm";
hmContextIds["44700"]="towerms_parasitics.htm";
hmContextIds["43000"]="dish_stirling.htm";
hmContextIds["45200"]="generic_system_plant.htm";
hmContextIds["300000"]="references.htm";
