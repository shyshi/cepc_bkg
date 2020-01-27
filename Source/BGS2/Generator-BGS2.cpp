#include<iostream>
#include<vector>
#include<random>
#include<algorithm>

double dsigmade(double e,double eta)
{
    int zz1=8;
    int zz2=6;
    double Fz1, Fz2;
    Fz1=pow(zz1,2.0)*log(183/pow(zz1,1/3))+zz1*log(1194/pow(zz1,2/3));
    Fz2=pow(zz2,2.0)*log(183/pow(zz2,1/3))+zz2*log(1194/pow(zz2,2/3));
    double dsigma1, dsigma2;
    dsigma1=1.0/e*((4.0/3*(1-e)+pow(e,2)*Fz1+1.0/9.0*zz1*(zz1+1)*log(1.0/eta)-1));
    dsigma2=1.0/e*((4.0/3*(1-e)+pow(e,2)*Fz2+1.0/9.0*zz2*(zz2+1)*log(1.0/eta)-1));
    return dsigma1+dsigma2;
}

int main()
{
    using namespace std;
    double eta = 0.015;
    //cin >> eta;
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
    randomChain.push_back(hlim);
    for (vector<double>::iterator it = randomChain.begin(); it != randomChain.end(); ++it)
    {
        cout << *it;
        cout << "\n";
    }
    /*sort(randomChain.begin(),randomChain.end());
    vector<double> xbin;
    xbin.push_back(*randomChain.begin());
    for (int i=1; i<nbins;i++)
    {
        xbin.push_back(*(randomChain.begin()+i)-*(randomChain.begin()+i-1));
    }
    vector<double> areaswap, area, results;
    for (int i=0;i<nbins;i++)
    {
        double temp;
        temp=dsigmade(*(randomChain.begin()+i),eta)*(*(xbin.begin()+i));
        areaswap.push_back(temp);
    }
    area.push_back(*areaswap.begin());
    for (int i=1;i<nbins;i++)
    {
        double temp;
        temp=*(area.begin()+i-1)+(*(areaswap.begin()+i));
        area.push_back(temp);
    }
    for (int i=0;i<nbins;i++)
    {
        double total=*area.end();
        *(area.begin()+i)=(*(area.begin()+i))/total;
    }
    for (vector<double>::iterator it=area.begin(); it!=area.end(); ++it)
    {
        cout << *it;
        cout << "\n";
    }*/
    return 0;
}
