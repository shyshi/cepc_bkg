#include<iostream>
#include<vector>
#include<random>
#include<ctime>

int main()
{
    using namespace std;
    double eta = 0.015;
    //cin >> eta;
    int zz1=8;
    int zz2=6;
    double Fz1, Fz2;
    Fz1=pow(zz1,2.0)*log(183/pow(zz1,1/3))+zz1*log(1194/pow(zz1,2/3));
    Fz2=pow(zz2,2.0)*log(183/pow(zz2,1/3))+zz2*log(1194/pow(zz2,2/3));
    int nn = 200000;
    double hlim=1, llim=eta;
    int nbins = 10000;
    random_device rd;
    mt19937 gen(rd());
    uniform_real_distribution<> dis(llim,hlim);
    vector<double> randomChain;
    double vRandom;
    for (int i=0;i<nbins-1;i++)
    {
        vRandom=dis(gen);
        randomChain.push_back(vRandom);

    }
    return 0;
}
