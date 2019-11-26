#include <gtest/gtest.h>
#include <string>

#include <variables.h>

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
	ssc_var_t data_arr = vv->AsSSCVar();
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
    ssc_var_t data_matt = vv->AsSSCVar();

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