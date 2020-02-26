#include <gtest/gtest.h>
#include <string>

#include <variables.h>
#include <lk/env.h>


TEST(VarTable_variables, Invalid)
{
    // ssc data into sam data
    auto var = ssc_var_create();
    VarValue vv = VarValue(var);
    EXPECT_EQ(vv.Type(), VV_INVALID);

    // sam back into ssc data
    auto data_returned = ssc_var_create();
    vv.AsSSCVar(data_returned);
    EXPECT_EQ(ssc_var_query(data_returned), SSC_INVALID);

    ssc_var_free(data_returned);
    ssc_var_free(var);
}

TEST(VarTable_variables, Binary)
{
    VarValue vv = VarValue();
    vv.ChangeType(VV_BINARY);

    // sam back into ssc data
    auto var_returned = ssc_var_create();
    bool success = vv.AsSSCVar(var_returned);
    EXPECT_FALSE(success);
    ssc_var_free(var_returned);
}

TEST(VarTable_variables, Number)
{
    // ssc data into sam data
    auto var = ssc_var_create();
    ssc_var_set_number(var, 10);
    VarValue vv = VarValue(var);
    EXPECT_EQ(vv.Type(), VV_NUMBER);
    EXPECT_EQ(vv.Value(), 10);

    // sam back into ssc data
    auto var_returned = ssc_var_create();
    vv.AsSSCVar(var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_NUMBER);
    EXPECT_EQ(ssc_var_get_number(var_returned), 10);

    ssc_var_free(var_returned);
    ssc_var_free(var);
}

TEST(VarTable_variables, String)
{
    // ssc data into sam data
    auto var = ssc_var_create();
    ssc_var_set_string(var, "string");
    VarValue vv = VarValue(var);
    EXPECT_EQ(vv.Type(), VV_STRING);
    EXPECT_EQ(vv.String(), "string");

    // sam back into ssc data
    auto var_returned = ssc_var_create();
    vv.AsSSCVar(var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_STRING);
    std::string str = ssc_var_get_string(var_returned);
    EXPECT_EQ(str, "string");

    ssc_var_free(var_returned);
    ssc_var_free(var);
}

TEST(VarTable_variables, Array)
{
    // ssc data into sam data
    auto var = ssc_var_create();

    ssc_number_t arr[2] = {2, 3};
    ssc_var_set_array(var, arr, 2);
    VarValue vv = VarValue(var);
    EXPECT_EQ(vv.Type(), VV_ARRAY);
    EXPECT_EQ(vv.Array()[0], 2);
    EXPECT_EQ(vv.Array()[1], 3);

    // sam back into ssc data
    auto var_returned = ssc_var_create();
    vv.AsSSCVar(var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_ARRAY);
    EXPECT_EQ(ssc_var_get_array(var_returned, nullptr)[0], 2);
    EXPECT_EQ(ssc_var_get_array(var_returned, nullptr)[1], 3);

    ssc_var_free(var_returned);
    ssc_var_free(var);
}

TEST(VarTable_variables, Matrix)
{
    // ssc data into sam data
    auto var = ssc_var_create();

    ssc_number_t arr[4] = {2, 3, 4, 5};
    ssc_var_set_matrix(var, arr, 2, 2);
    VarValue vv = VarValue(var);
    EXPECT_EQ(vv.Type(), VV_MATRIX);
    EXPECT_EQ(vv.Matrix().at(0, 0), 2);
    EXPECT_EQ(vv.Matrix().at(0, 1), 3);
    EXPECT_EQ(vv.Matrix().at(1, 0), 4);
    EXPECT_EQ(vv.Matrix().at(1, 1), 5);

    // sam back into ssc data
    auto var_returned = ssc_var_create();
    vv.AsSSCVar(var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_MATRIX);
    EXPECT_EQ(ssc_var_get_matrix(var_returned, nullptr, nullptr)[0], 2);
    EXPECT_EQ(ssc_var_get_matrix(var_returned, nullptr, nullptr)[1], 3);
    EXPECT_EQ(ssc_var_get_matrix(var_returned, nullptr, nullptr)[2], 4);
    EXPECT_EQ(ssc_var_get_matrix(var_returned, nullptr, nullptr)[3], 5);

    ssc_var_free(var_returned);
    ssc_var_free(var);
}

TEST(VarTable_variables, Table)
{
    // ssc data into sam data
    auto table = ssc_data_create();
    ssc_data_set_string(table, "string", "string");

    auto var = ssc_var_create();
    ssc_var_set_table(var, table);
    VarValue vv = VarValue(var);
    EXPECT_EQ(vv.Type(), VV_TABLE);
    EXPECT_EQ(vv.Table().Get("string")->String(), "string");

    // sam back into ssc data
    auto var_returned = ssc_var_create();
    vv.AsSSCVar(var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_TABLE);

    auto table_returned = ssc_var_get_table(var_returned);
    std::string str = ssc_data_get_string(table_returned, "string");
    EXPECT_EQ(str, "string");

    ssc_var_free(var_returned);
    ssc_data_free(table);
    ssc_var_free(var);
}

TEST(VarTable_variables, DataArray)
{
    ssc_var_t vd[2];
    for (size_t i = 0; i < 2; i++){
        vd[i] = ssc_var_create();
        ssc_var_set_number(vd[i], 2 + i);
    }

    // set using ssc_data
    auto data = ssc_data_create();
    ssc_data_set_data_array(data, "array", &vd[0], 2);

    VarTable variables = VarTable(data);
    auto vv = variables.Get("array");
	auto arr = vv->DataArray();
	EXPECT_EQ(arr[0].Value(), 2);
	EXPECT_EQ(arr[1].Value(), 3);

	// test getting back as ssc_data
	ssc_var_t data_arr = ssc_var_create();
	vv->AsSSCVar(data_arr);
	EXPECT_EQ(ssc_var_get_number(ssc_var_get_var_array(data_arr, 0)), 2);
    EXPECT_EQ(ssc_var_get_number(ssc_var_get_var_array(data_arr, 1)), 3);

	// test string
	std::string str = vv->AsString(',', ',');
	EXPECT_EQ(str, "2,3");

	for (size_t i = 0; i < 2; i++)
	    ssc_var_free(vd[i]);
	ssc_data_free(data);
	ssc_var_free(data_arr);
}

TEST(VarTable_variables, DataMatrix)
{
    // create data entries
    ssc_var_t vd[4];
    for (size_t i = 0; i < 4; i++){
        vd[i] = ssc_var_create();
        ssc_var_set_number(vd[i], 2 + i);
    }

    // set using ssc_data
    auto data = ssc_data_create();
    ssc_data_set_data_matrix(data, "matrix", &vd[0], 2, 2);

    VarTable variables = VarTable(data);
    auto vv = variables.Get("matrix");
    auto mat = vv->DataMatrix();
    EXPECT_EQ(mat[0][0].Value(), 2);
    EXPECT_EQ(mat[0][1].Value(), 3);
    EXPECT_EQ(mat[1][0].Value(), 4);
    EXPECT_EQ(mat[1][1].Value(), 5);

    // test getting back as var_data
    ssc_var_t data_matt = ssc_var_create();
    vv->AsSSCVar(data_matt);

    EXPECT_EQ(ssc_var_get_number(ssc_var_get_var_matrix(data_matt, 0, 0)), 2);
    EXPECT_EQ(ssc_var_get_number(ssc_var_get_var_matrix(data_matt, 0, 1)), 3);
    EXPECT_EQ(ssc_var_get_number(ssc_var_get_var_matrix(data_matt, 1, 0)), 4);
    EXPECT_EQ(ssc_var_get_number(ssc_var_get_var_matrix(data_matt, 1, 1)), 5);

    // test string
    std::string str = vv->AsString(',', ',');
    assert(str == "[2,3][4,5]");

    for (size_t i = 0; i < 4; i++)
        ssc_var_free(vd[i]);
    ssc_data_free(data);
    ssc_var_free(data_matt);
}


TEST(LK_SSC_invoke, Invalid)
{
    // ssc data into lk data
    auto var = ssc_var_create();
    lk::vardata_t vd;
    sscvar_to_lkvar(var, vd);
    EXPECT_EQ(vd.type(), (unsigned char)lk::vardata_t::NULLVAL);

    // lk back into ssc data
    auto data_returned = ssc_var_create();
    lkvar_to_sscvar(vd, var);
    EXPECT_EQ(ssc_var_query(data_returned), SSC_INVALID);

    ssc_var_free(data_returned);
    ssc_var_free(var);
}

TEST(LK_SSC_invoke, Reference)
{
    // lk data into ssc data
    lk::vardata_t vd, vd_refd;
    vd_refd.assign(10);
    vd.assign(&vd_refd);

    auto var = ssc_var_create();
    lkvar_to_sscvar(vd, var);
    EXPECT_EQ(ssc_var_query(var), SSC_NUMBER);
    EXPECT_EQ(ssc_var_get_number(var), 10);

    // ssc data into lk as not reference but value
    auto data_returned = ssc_var_create();
    sscvar_to_lkvar(var, vd);
    EXPECT_EQ(vd.type(), (unsigned char)lk::vardata_t::NUMBER);
    EXPECT_EQ(vd.num(), 10);

    ssc_var_free(data_returned);
    ssc_var_free(var);
}

TEST(LK_SSC_invoke, Number)
{
    // lk data into ssc data
    lk::vardata_t vd;
    vd.assign(10);

    auto var = ssc_var_create();
    lkvar_to_sscvar(vd, var);
    EXPECT_EQ(ssc_var_query(var), SSC_NUMBER);
    EXPECT_EQ(ssc_var_get_number(var), 10);

    // ssc data into lk as not reference but value
    auto data_returned = ssc_var_create();
    sscvar_to_lkvar(var, vd);
    EXPECT_EQ(vd.type(), (unsigned char)lk::vardata_t::NUMBER);
    EXPECT_EQ(vd.num(), 10);

    ssc_var_free(data_returned);
    ssc_var_free(var);
}

TEST(LK_SSC_invoke, String)
{
    // lk data into ssc data
    lk::vardata_t vd, vd_returned;
    vd.assign("string");

    auto var = ssc_var_create();
    lkvar_to_sscvar(vd, var);
    EXPECT_EQ(ssc_var_query(var), SSC_STRING);
    std::string str = ssc_var_get_string(var);
    EXPECT_EQ(str, "string");

    // ssc data into lk as not reference but value
    sscvar_to_lkvar(var, vd_returned);
    EXPECT_EQ(vd_returned.type(), (unsigned char)lk::vardata_t::STRING);
    EXPECT_EQ(vd_returned.str(), "string");

    ssc_var_free(var);
}

TEST(LK_SSC_invoke, NumVector)
{
    // ssc data into lk data
    auto var = ssc_var_create();
    ssc_number_t arr[2] = {2, 3};
    ssc_var_set_array(var, arr, 2);

    lk::vardata_t vd;
    sscvar_to_lkvar(var, vd);
    EXPECT_EQ(vd.type(), (unsigned char)lk::vardata_t::VECTOR);
    EXPECT_EQ((*vd.vec())[0].num(), 2);
    EXPECT_EQ((*vd.vec())[1].num(), 3);

    // lk back into ssc data
    auto var_returned = ssc_var_create();
    lkvar_to_sscvar(vd, var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_ARRAY);
    EXPECT_EQ(ssc_var_get_array(var_returned, nullptr)[0], 2);
    EXPECT_EQ(ssc_var_get_array(var_returned, nullptr)[1], 3);

    ssc_var_free(var_returned);
    ssc_var_free(var);
}

TEST(LK_SSC_invoke, NumMatrix)
{
    // ssc data into lk data
    auto var = ssc_var_create();
    ssc_number_t arr[4] = {2, 3, 4, 5};
    ssc_var_set_matrix(var, arr, 2, 2);

    lk::vardata_t vd;
    sscvar_to_lkvar(var, vd);
    EXPECT_EQ(vd.type(), (unsigned char)lk::vardata_t::VECTOR);
    auto vec = *vd.vec();
    EXPECT_EQ(vec[0].type(), (unsigned char)lk::vardata_t::VECTOR);
    auto vec_col = *(vec[0].vec());
    EXPECT_EQ(vec_col[0].num(), 2);
    EXPECT_EQ(vec_col[1].num(), 3);
    vec_col = *(vec[1].vec());
    EXPECT_EQ(vec_col[0].num(), 4);
    EXPECT_EQ(vec_col[1].num(), 5);

    // lk back into ssc data
    auto var_returned = ssc_var_create();
    lkvar_to_sscvar(vd, var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_MATRIX);
    auto mat = ssc_var_get_matrix(var_returned, nullptr, nullptr);
    EXPECT_EQ(mat[0], 2);
    EXPECT_EQ(mat[1], 3);
    EXPECT_EQ(mat[2], 4);
    EXPECT_EQ(mat[3], 5);

    ssc_var_free(var_returned);
    ssc_var_free(var);
}

TEST(LK_SSC_invoke, Hash)
{
    // ssc data into lk data
    auto table = ssc_data_create();
    ssc_data_set_string(table, "string", "string");
    auto var = ssc_var_create();
    ssc_var_set_table(var, table);

    lk::vardata_t vd;
    sscvar_to_lkvar(var, vd);
    EXPECT_EQ(vd.type(), (unsigned char)lk::vardata_t::HASH);
    EXPECT_EQ(vd.lookup("string")->str(), "string");

    // lk back into ssc data
    auto var_returned = ssc_var_create();
    lkvar_to_sscvar(vd, var_returned);
    EXPECT_EQ(ssc_var_query(var_returned), SSC_TABLE);

    auto table_returned = ssc_var_get_table(var_returned);
    std::string str = ssc_data_get_string(table_returned, "string");
    EXPECT_EQ(str, "string");

    ssc_var_free(var_returned);
    ssc_data_free(table);
    ssc_var_free(var);
}

TEST(LK_SSC_invoke, DataVector)
{
    ssc_var_t entry[2];
    for (size_t i = 0; i < 2; i++){
        entry[i] = ssc_var_create();
        ssc_var_set_string(entry[i], std::to_string(i + 2).c_str());
    }

    // ssc to lk data
    auto var = ssc_var_create();
    for (size_t i = 0; i < 2; i++)
        ssc_var_set_data_array(var, entry[i], i);

    lk::vardata_t vd;
    sscvar_to_lkvar(var, vd);
    auto vec = *vd.vec();
    EXPECT_EQ(vec[0].str(), "2");
    EXPECT_EQ(vec[1].str(), "3");

    // test getting back as ssc_data
    ssc_var_t data_arr = ssc_var_create();
    lkvar_to_sscvar(vd, data_arr);
    std::string str = ssc_var_get_string(ssc_var_get_var_array(data_arr, 0));
    EXPECT_EQ(str, "2");
    str = ssc_var_get_string(ssc_var_get_var_array(data_arr, 1));
    EXPECT_EQ(str, "3");

    ssc_var_free(var);
    ssc_var_free(entry[0]);
    ssc_var_free(entry[1]);
    ssc_var_free(data_arr);
}

TEST(LK_SSC_invoke, DataMatrix)
{
    // ssc to lk data
    auto var = ssc_var_create();
    ssc_var_t entry[4];
    for (size_t i = 0; i < 4; i++){
        entry[i] = ssc_var_create();
        ssc_var_set_string(entry[i], std::to_string(i + 2).c_str());
    }

    // set using ssc_data
    for (size_t i = 0; i < 2; i++)
        for (size_t j = 0; j < 2; j++)
            ssc_var_set_data_matrix(var, entry[i * 2 + j], i, j);

    lk::vardata_t vd;
    sscvar_to_lkvar(var, vd);
    auto row = *vd.vec();
    EXPECT_EQ(row[0].type(), (unsigned char)lk::vardata_t::VECTOR);
    auto col = *(row[0].vec());
    EXPECT_EQ(col[0].type(), (unsigned char)lk::vardata_t::STRING);
    EXPECT_EQ(col[0].str(), "2");
    EXPECT_EQ(col[1].str(), "3");
    col = *(row[1].vec());
    EXPECT_EQ(col[0].str(), "4");
    EXPECT_EQ(col[1].str(), "5");

    // test getting back as ssc
    ssc_var_t data_matt = ssc_var_create();
    lkvar_to_sscvar(vd, data_matt);

    std::string str = ssc_var_get_string(ssc_var_get_var_matrix(data_matt, 0, 0));
    EXPECT_EQ(str, "2");
    str = ssc_var_get_string(ssc_var_get_var_matrix(data_matt, 0, 1));
    EXPECT_EQ(str, "3");
    str = ssc_var_get_string(ssc_var_get_var_matrix(data_matt, 1, 0));
    EXPECT_EQ(str, "4");
    str = ssc_var_get_string(ssc_var_get_var_matrix(data_matt, 1, 1));
    EXPECT_EQ(str, "5");

    for (size_t i = 0; i < 4; i++)
        ssc_var_free(entry[i]);
    ssc_var_free(var);
    ssc_var_free(data_matt);
}
