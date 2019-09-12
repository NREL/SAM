#include <gtest/gtest.h>
#include <string>

#include <ssc/vartab.h>
#include <variables.h>

TEST(VarValueTests, DataArray)
{
	// test creating from var_data
	std::vector<var_data> vd;
	vd.resize(2);
	for (auto& i : vd){
	    i.type = SSC_NUMBER;
	    i.num = 2;
	}
	auto vd_vec = var_data(vd);
	VarValue vv = VarValue(&vd_vec);
	auto arr = vv.DataArray();
	assert(arr[0].Value() == 2 && arr[1].Value() == 2);

	// test getting back as var_data
	vd_vec = vv.AsSSCVar();
	assert(vd_vec.vec[0].num == 2 && vd_vec.vec[1].num == 2);

	// test string
	std::string str = vv.AsString(',', ',');
	assert(str == "2,2");
}

TEST(VarValueTests, DataMatrix)
{
    // test creating from var_data
    std::vector<var_data> vd;
    vd.resize(2);
    for (auto& i : vd){
        i.type = SSC_NUMBER;
        i.num = 2;
    }
    std::vector<std::vector<var_data>> vm;
    vm.push_back(vd);
    vm.push_back(vd);

    auto vd_mat = var_data(vm);
    VarValue vv = VarValue(&vd_mat);
    auto mat = vv.DataMatrix();
    assert(mat[0][0].Value() == 2 && mat[0][1].Value() == 2);
    assert(mat[0][0].Value() == 2 && mat[0][1].Value() == 2);

    // test getting back as var_data
    vd_mat = vv.AsSSCVar();
    assert(vd_mat.mat[0][0].num == 2 && vd_mat.mat[0][1].num == 2);
    assert(vd_mat.mat[1][0].num == 2 && vd_mat.mat[1][1].num == 2);

    // test string
    std::string str = vv.AsString(',', ',');
    assert(str == "[2,2][2,2]");
}