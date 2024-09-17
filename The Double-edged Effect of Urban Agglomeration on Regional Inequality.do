///////////////////---以城市为单元--邻域城市（仅保留同省同城市群）----------------------------------------------------------------

use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
//变量描述性统计分析
asdoc summarize TWRt密度 TBRt密度 Theilt密度 DID 政府干预 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, stat(N mean sd p25 p50 p75 tstat) fs(7) dec(4)

//变量相关性分析
asdoc pwcorr_a TWRt密度 TBRt密度 Theilt密度 DID 政府干预 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,star1(0.01) star5(0.05) star10(0.1)
 
//方差膨胀因子
reg Theilt密度 DID 政府干预 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积
estat vif

/****** Baseline Regression ******/
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
reghdfe TWRt密度 DID ,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 DID ,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 DID ,absorb(year id) vce(cluster id)
est store m3

reghdfe TWRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6

esttab m1 m2 m3 m4 m5 m6 using baseline.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep  (DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 ) order (DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 ) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap

/****** Inverted U-shape analysis ******/
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
gen 暴露时间 = year - treat_date if treat == 1
replace 暴露时间 = 0 if 暴露时间 ==.
gen 暴露时间2 = 暴露时间^2
gen DID暴露时间 = DID*暴露时间
gen DID暴露时间2 = DID*暴露时间2

xtset id year
gen event = year - treat_date if treat == 1
replace event = -4 if event <= -4
tab event, gen(eventz)
forvalue i = 1/9{
replace eventz`i' = 0 if eventz`i' == .
}

reghdfe TWRt密度 eventz5 eventz6 eventz7 eventz8 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 eventz5 eventz6 eventz7 eventz8 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 eventz5 eventz6 eventz7 eventz8 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m3

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp  T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp  T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6

esttab m1 m2 m3 m4 m5 m6 using baselineU.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep  (eventz5 eventz6 eventz7 eventz8 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2) order (eventz5 eventz6 eventz7 eventz8 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap

/****** Mechanism Analysis ******/
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
reghdfe T投资水平 DID,absorb(year id) vce(cluster id)
est store m1
reghdfe T人力资本 DID,absorb(year id) vce(cluster id)
est store m2
reghdfe T科技支出占比 DID,absorb(year id) vce(cluster id)
est store m3
reghdfe T投资水平 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe T人力资本 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe T科技支出占比 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6

esttab m1 m2 m3 m4 m5 m6 using MechanismA.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep ( DID ) order ( DID ) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap


reghdfe 投资水平 DID,absorb(year id) vce(cluster id)
est store m1
reghdfe 人力资本 DID,absorb(year id) vce(cluster id)
est store m2
reghdfe 科技支出占比 DID,absorb(year id) vce(cluster id)
est store m3
reghdfe 投资水平 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe 人力资本 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe 科技支出占比 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6

esttab m1 m2 m3 m4 m5 m6 using MechanismB.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep ( DID ) order ( DID ) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap




use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
reghdfe TWRt密度 DID T投资水平 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 DID T投资水平 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 DID T投资水平 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m3
reghdfe TWRt密度 DID T人力资本 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID T人力资本 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID T人力资本 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6
reghdfe TWRt密度 DID T科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m7
reghdfe TBRt密度 DID T科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m8
reghdfe Theilt密度 DID T科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m9
reghdfe TWRt密度 DID T投资水平 T人力资本 T科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m10
reghdfe TBRt密度 DID T投资水平 T人力资本 T科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m11
reghdfe Theilt密度 DID T投资水平 T人力资本 T科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m12

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 using MechanismA.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep ( DID ) order ( DID ) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap


use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
reghdfe TWRt密度 DID 投资水平 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 DID 投资水平 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 DID 投资水平 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m3
reghdfe TWRt密度 DID 人力资本 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID 人力资本 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID 人力资本 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6
reghdfe TWRt密度 DID 科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m7
reghdfe TBRt密度 DID 科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m8
reghdfe Theilt密度 DID 科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m9
reghdfe TWRt密度 DID 投资水平 人力资本 科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m10
reghdfe TBRt密度 DID 投资水平 人力资本 科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m11
reghdfe Theilt密度 DID 投资水平 人力资本 科技支出占比 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m12

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 using MechanismB.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep ( DID ) order ( DID ) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap

/****** 政府干预 ******/
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
gen 暴露时间 = year - treat_date if treat == 1
replace 暴露时间 = 0 if 暴露时间 ==.
gen 暴露时间2 = 暴露时间^2
gen DID暴露时间 = DID*暴露时间
gen DID暴露时间2 = DID*暴露时间2

gen DID暴露时间政府干预 = DID暴露时间*政府干预
gen DID暴露时间2政府干预 = DID暴露时间2*政府干预

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 政府干预 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 政府干预 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 政府干预 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m3
reghdfe TWRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 T政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6

esttab m1 m2 m3 m4 m5 m6 using Government.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep (DID暴露时间 DID暴露时间2  DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) order (DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap

/****** Parallel Trend Test ******/
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
xtset id year
gen event = year - treat_date if treat == 1
replace event = -4 if event <= -4
tab event, gen(eventz)
forvalue i = 1/9{
replace eventz`i' = 0 if eventz`i' == .
}
drop eventz1

reghdfe TWRt密度 eventz* T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
coefplot,baselevels keep(event*) vertical coeflabels(eventz1 = "-4" eventz2 = "-3" eventz3 = "-2" eventz4 = "-1" eventz5 = "0" eventz6 = "1" eventz7 = "2" eventz8 = "3" eventz9 = "4") title("Parallel trend test: intra-INEQ", size(*0.8)) yline(0, lc(black*0.5) lp(dash)) xline(4, lc(black*0.5) lp(dash)) ylabel(, nogrid format(%6.2f) labsize(small))  ytitle("Coefficients", size(*0.8)) xtitle("Time passage relative to year of UAP", size(*0.8)) ciopts( lpattern(dash) recast(rcap) msize(small)) msymbol(circle_hollow) msymbol(O) msize(small) mcolor(navy8) addplot(line @b @at,lwidth(sthick) lcolor(navy8)) graphregion(color(white))
graph save "平行趋势检验TWR", replace

reghdfe TBRt密度 eventz* T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
coefplot,baselevels keep(event*) vertical coeflabels(eventz1 = "-4" eventz2 = "-3" eventz3 = "-2" eventz4 = "-1" eventz5 = "0" eventz6 = "1" eventz7 = "2" eventz8 = "3" eventz9 = "4") title("Parallel trend test: inter-INEQ", size(*0.8)) yline(0, lc(black*0.5) lp(dash)) xline(4, lc(black*0.5) lp(dash)) ylabel(, nogrid format(%6.2f) labsize(small))  ytitle("Coefficients", size(*0.8)) xtitle("Time passage relative to year of UAP", size(*0.8)) ciopts( lpattern(dash) recast(rcap) msize(small)) msymbol(circle_hollow) msymbol(O) msize(small) mcolor(navy8) addplot(line @b @at,lwidth(sthick) lcolor(navy8)) graphregion(color(white))
graph save "平行趋势检验TBR", replace

reghdfe Theilt密度 eventz* T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
coefplot,baselevels keep(event*) vertical coeflabels(eventz1 = "-4" eventz2 = "-3" eventz3 = "-2" eventz4 = "-1" eventz5 = "0" eventz6 = "1" eventz7 = "2" eventz8 = "3" eventz9 = "4") title("Parallel trend test: INEQ", size(*0.8)) yline(0, lc(black*0.5) lp(dash)) xline(4, lc(black*0.5) lp(dash)) ylabel(, nogrid format(%6.2f) labsize(small))  ytitle("Coefficients", size(*0.8)) xtitle("Time passage relative to year of UAP", size(*0.8)) ciopts( lpattern(dash) recast(rcap) msize(small)) msymbol(circle_hollow) msymbol(O) msize(small) mcolor(navy8) addplot(line @b @at,lwidth(sthick) lcolor(navy8)) graphregion(color(white))
graph save "平行趋势检验Theil", replace

graph combine 平行趋势检验TWR.gph 平行趋势检验TBR.gph 平行趋势检验Theil.gph, row(1) graphregion(color(white))

/****** Placebo Test ******/
///Theil

clear
forvalues k = 1/500{
mat b = J(500,1,0)
mat se = J(500,1,0)
mat p = J(500,1,0)
forvalues i = 1/500{
  use "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta",clear
  duplicates drop id, force
  keep if year >= 2011
  xtset id year
  keep if year == 2008
  sample 158,count
  keep id
  save matchid.dta,replace
  merge 1:m id using "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta"
  keep if year >= 2011
  gen treat=(_merge == 3)
  save matchid`i'.dta,replace
  
  use "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta",clear

  bsample 1, strata(id)
  keep year
  save matchyear.dta,replace
  mkmat year, matrix(sampleyear)
  
  use matchid`i'.dta,replace
  xtset id year
  gen time = 0
  foreach j of numlist 1/272 {
  replace time = 1 if (id == `j' &  year >= sampleyear[`j',1])
  }
 
  gen DIDD=time*treat  
  reghdfe Theilt密度 DIDD T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
  mat b[`i', 1] = _b[DIDD]
  mat se[`i', 1] = _se[DIDD]
  scalar df_r = e(N) - e(df_m) - 1
  mat p[`i', 1] = 2*ttail(df_r,abs(_b[DIDD]/_se[DIDD]))
}
 
svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)

drop if pvalue == .
label var pvalue
label var coef
keep coef se pvalue

twoway (scatter pvalue coef, msymbol(smcircle_hollow) mcolor(navy)) (kdensity coef, yaxis(2)), title("Placebo Test: Theil-T", size(*0.8)) xline(-0.0001, lc(black*0.5) lp(dash)) xline(0, lc(black*0.5) lp(solid)) yline(0.05, lc(black*0.5) lp(dash)) xlabel(-0.010(0.005)0.010) xtitle("Coefficients", size(*0.8)) xlabel(, format(%05.3f) labsize(small)) ytitle("P Value", size(*0.8)) ylabel(, nogrid format(%4.1f) labsize(small)) ytitle("Kdensity of Estimates", size(*0.8) axis(2)) ylabel(, nogrid format(%4.1f) labsize(small) axis(2)) legend(r(1) order(1 "P Value" 2 "Kdensity of Estimates")) graphregion(color(white))
graph save "安慰剂检验Theil`k'", replace
forvalue i=1/500{
  erase  matchid`i'.dta 
}
erase  matchid.dta 
erase  matchyear.dta 
}

///TWRt
clear
forvalues k = 1/500{
mat b = J(500,1,0)
mat se = J(500,1,0)
mat p = J(500,1,0)
forvalues i = 1/500{
  use "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta",clear
  keep if year >= 2011
  xtset id year
  keep if year == 2008
  sample 158,count
  keep id
  save matchid.dta,replace
  merge 1:m id using "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta"
  keep if year >= 2011
  gen treat=(_merge == 3)
  save matchid`i'.dta,replace
  
  use "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta",clear
  keep if year >= 2011
  bsample 1, strata(id)
  keep year
  save matchyear.dta,replace
  mkmat year, matrix(sampleyear)
  
  use matchid`i'.dta,replace
  xtset id year
  gen time = 0
  foreach j of numlist 1/272 {
  replace time = 1 if (id == `j' &  year >= sampleyear[`j',1])
  }
 
  gen DIDD=time*treat  
  reghdfe TWRt密度 DIDD T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
  mat b[`i', 1] = _b[DIDD]
  mat se[`i', 1] = _se[DIDD]
  scalar df_r = e(N) - e(df_m) - 1
  mat p[`i', 1] = 2*ttail(df_r,abs(_b[DIDD]/_se[DIDD]))
}
 
svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)

drop if pvalue == .
label var pvalue
label var coef
keep coef se pvalue

twoway (scatter pvalue coef, msymbol(smcircle_hollow) mcolor(navy)) (kdensity coef, yaxis(2)), title("Placebo Test: Theil-T_W", size(*0.8)) xline(0.0067, lc(black*0.5) lp(dash)) xline(0, lc(black*0.5) lp(solid)) yline(0.05, lc(black*0.5) lp(dash)) xlabel(-0.008(0.004)0.008) xtitle("Coefficients", size(*0.8)) xlabel(, format(%05.3f) labsize(small)) ytitle("P Value", size(*0.8)) ylabel(, nogrid format(%4.1f) labsize(small)) ytitle("Kdensity of Estimates", size(*0.8) axis(2)) ylabel(, nogrid format(%4.1f) labsize(small) axis(2)) legend(r(1) order(1 "P Value" 2 "Kdensity of Estimates")) graphregion(color(white))
graph save "安慰剂检验TWR`k'", replace
forvalue i=1/500{
  erase  matchid`i'.dta 
}
erase  matchid.dta 
erase  matchyear.dta 
}

///TBRt
clear
forvalues k = 1/500{
mat b = J(500,1,0)
mat se = J(500,1,0)
mat p = J(500,1,0)
forvalues i = 1/500{
  use "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta",clear
  keep if year >= 2011
  xtset id year
  keep if year == 2008
  sample 158,count
  keep id
  save matchid.dta,replace
  merge 1:m id using "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta"
  keep if year >= 2011
  gen treat=(_merge == 3)
  save matchid`i'.dta,replace
  
  use "/Users/apple/Desktop/区域经济 论文/出图/纯泰尔指数邻域同省面板数据安慰剂检验灯光密度1.dta",clear
  keep if year >= 2011
  bsample 1, strata(id)
  keep year
  save matchyear.dta,replace
  mkmat year, matrix(sampleyear)
  
  use matchid`i'.dta,replace
  xtset id year
  gen time = 0
  foreach j of numlist 1/272 {
  replace time = 1 if (id == `j' &  year >= sampleyear[`j',1])
  }
 
  gen DIDD=time*treat  
  reghdfe TBRt密度 DIDD T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id) 
  mat b[`i', 1] = _b[DIDD]
  mat se[`i', 1] = _se[DIDD]
  scalar df_r = e(N) - e(df_m) - 1
  mat p[`i', 1] = 2*ttail(df_r,abs(_b[DIDD]/_se[DIDD]))
}
 
svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)

drop if pvalue == .
label var pvalue
label var coef
keep coef se pvalue


twoway (scatter pvalue coef, msymbol(smcircle_hollow) mcolor(navy)) (kdensity coef, yaxis(2)), title("Placebo Test: Theil-T_B", size(*0.8)) xline(-0.0069, lc(black*0.5) lp(dash)) xline(0, lc(black*0.5) lp(solid)) yline(0.05, lc(black*0.5) lp(dash)) xlabel(-0.008(0.004)0.008) xtitle("Coefficients", size(*0.8)) xlabel(, format(%05.3f) labsize(small)) ytitle("P Value", size(*0.8)) ylabel(, nogrid format(%4.1f) labsize(small)) ytitle("Kdensity of Estimates", size(*0.8) axis(2)) ylabel(, nogrid format(%4.1f) labsize(small) axis(2)) legend(r(1) order(1 "P Value" 2 "Kdensity of Estimates")) graphregion(color(white))
graph save "安慰剂检验TBR`k'", replace
forvalue i=1/500{
  erase  matchid`i'.dta 
}
erase  matchid.dta 
erase  matchyear.dta 
}



graph combine 安慰剂检验TWR.gph 安慰剂检验TBR.gph 安慰剂检验Theil.gph , graphregion(color(white))

/****** Robustness Analysis ******/

////////Extend the sample time window--------------------------
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
gen 暴露时间 = year - treat_date if treat == 1
replace 暴露时间 = 0 if 暴露时间 ==.
gen 暴露时间2 = 暴露时间^2
gen DID暴露时间 = DID*暴露时间
gen DID暴露时间2 = DID*暴露时间2

gen DID暴露时间政府干预 = DID暴露时间*政府干预
gen DID暴露时间2政府干预 = DID暴露时间2*政府干预

reghdfe TWRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面,absorb(year id) vce(cluster id)
est store m3

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m7
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m8
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m9

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 using 2008baseline.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep  (DID DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) order (DID DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap

////////Adjusting the policy implementation time for special samples--------------------------
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011

replace treat_date = 2015 if 城市群省份 =="3"
replace DID = 1 if 城市群省份 =="3" & year >= 2015

gen 暴露时间 = year - treat_date if treat == 1
replace 暴露时间 = 0 if 暴露时间 ==.
gen 暴露时间2 = 暴露时间^2
gen DID暴露时间 = DID*暴露时间
gen DID暴露时间2 = DID*暴露时间2

gen DID暴露时间政府干预 = DID暴露时间*政府干预
gen DID暴露时间2政府干预 = DID暴露时间2*政府干预

reghdfe TWRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面,absorb(year id) vce(cluster id)
est store m3

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m6

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m7
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m8
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
est store m9

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 using baselineGBA.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep  (DID DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) order (DID DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap

////////PSM-DID--------------------------
/////逐年k近邻匹配(k=10)
forvalue i = 2011/2019{
 use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
 keep if year == `i'
 set seed 10000
 gen tmp = runiform()
 sort tmp
 psmatch2 treat T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, neighbor(10) common odds logit ties ate
 save `i'.dta, replace
}

clear all

use 2011.dta, clear

forvalue k =2012/2019{
 append using `k'.dta
}

save ybyTheil10近邻匹配.dta, replace

///核密度图
*- 匹配前
sum _pscore if treat == 1, detail 
// 处理组的倾向得分均值为0.6101493

sum _pscore if treat == 0, detail
// 控制组的倾向得分均值为0.0952245

*- 匹配后
sum _pscore if treat == 1 & _weight != ., detail
// 处理组的倾向得分均值为0.3981038

sum _pscore if treat == 0 & _weight != ., detail
// 控制组的倾向得分均值为0.1319837

twoway(kdensity _pscore if treat == 1, lpattern(solid) lcolor(black) lwidth(thin) scheme(burd) ytitle("Nuclear density", size(5)) xtitle("Propensity score", size(5)) xline(0.6101493, lpattern(solid) lcolor(black) lwidth(thin))) (kdensity _pscore if treat == 0, lpattern(dash) lwidth(thin) xline(0.0952245, lpattern(dash) lwidth(thin))) , title("Propensity Score Before Matching", size(5)) xlabel(-2(1)5, labsize(5) format(%02.1f)) ylabel(0(5)0.5, labsize(5)) legend(label(1 "treated") label(2 "control") col(1) size(5) ring(0) position(1) symxsize(10)) graphregion(color(white))
graph save "qian", replace

twoway(kdensity _pscore if treat == 1 & _weight != ., lpattern(solid) lcolor(black) lwidth(thin) scheme(burd)  ytitle("Nuclear density", size(5)) xtitle("Propensity score", size(5)) xline(0.3981038, lpattern(solid) lcolor(black) lwidth(thin))) (kdensity _pscore if treat == 0 & _weight != ., lpattern(dash) lwidth(thin) xline(0.1319837, lpattern(dash) lwidth(thin))), title("Propensity Score After Matching", size(5)) xlabel(-2(1)5, labsize(5) format(%02.1f)) ylabel(0(5)0.5, labsize(5)) legend(label(1 "treated") label(2 "control") size(5) col(1) ring(0) position(1) symxsize(10)) graphregion(color(white))
graph save "hou", replace

graph combine qian.gph hou.gph, graphregion(color(white)) cols(1) xcom
graph save "临近10匹配", replace

///平衡图
pstest T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, both graph graphregion(color(white))
graph save "平衡图", replace

graph combine 临近10匹配.gph 平衡图.gph, row(1) graphregion(color(white))
graph save "匹配图", replace

///再次回归
gen 暴露时间 = year - treat_date if treat == 1
replace 暴露时间 = 0 if 暴露时间 ==.
gen 暴露时间2 = 暴露时间^2
gen DID暴露时间 = DID*暴露时间
gen DID暴露时间2 = DID*暴露时间2

gen DID暴露时间政府干预 = DID暴露时间*政府干预
gen DID暴露时间2政府干预 = DID暴露时间2*政府干预

reghdfe TWRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m1
reghdfe TBRt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m2
reghdfe Theilt密度 DID T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m3

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m4
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m5
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m6

reghdfe TWRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m7
reghdfe TBRt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m8
reghdfe Theilt密度 DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2 T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积 if _weight != .,absorb(year id) vce(cluster id)
est store m9

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 using AMk10baseline.rtf,star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) t(%6.2f) stats(N N_clust r2_within, labels("N" "Number of Regions" "Adjusted $R^2$") fmt(%9.0fc %9.0fc %9.4fc)) keep  (DID DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) order (DID DID暴露时间 DID暴露时间2 DID暴露时间政府干预 DID暴露时间2政府干预 政府干预 暴露时间 暴露时间2) coeflabels(_cons "Constant")  eqlabels("") substitute(\centering \centering\scriptsize)  replace nogap

///平行趋势检验
drop if _weight ==.
xtset id year
gen event = year - treat_date if treat == 1
replace event = -4 if event <= -4
tab event, gen(eventz)
forvalue i = 1/9{
replace eventz`i' = 0 if eventz`i' == .
}
drop eventz1

reghdfe TWRt密度 eventz* T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
coefplot,baselevels keep(event*) vertical coeflabels(eventz1 = "-4" eventz2 = "-3" eventz3 = "-2" eventz4 = "-1" eventz5 = "0" eventz6 = "1" eventz7 = "2" eventz8 = "3" eventz9 = "4") title("Parallel trend test: intra-INEQ", size(*0.8)) yline(0, lc(black*0.5) lp(dash)) xline(4, lc(black*0.5) lp(dash)) ylabel(, nogrid format(%6.2f) labsize(small))  ytitle("Coefficients", size(*0.8)) xtitle("Time passage relative to year of UAP", size(*0.8)) ciopts( lpattern(dash) recast(rcap) msize(small)) msymbol(circle_hollow) msymbol(O) msize(small) mcolor(navy8) addplot(line @b @at,lwidth(sthick) lcolor(navy8)) graphregion(color(white))
graph save "平行趋势检验TWR", replace

reghdfe TBRt密度 eventz* T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
coefplot,baselevels keep(event*) vertical coeflabels(eventz1 = "-4" eventz2 = "-3" eventz3 = "-2" eventz4 = "-1" eventz5 = "0" eventz6 = "1" eventz7 = "2" eventz8 = "3" eventz9 = "4") title("Parallel trend test: inter-INEQ", size(*0.8)) yline(0, lc(black*0.5) lp(dash)) xline(4, lc(black*0.5) lp(dash)) ylabel(, nogrid format(%6.2f) labsize(small))  ytitle("Coefficients", size(*0.8)) xtitle("Time passage relative to year of UAP", size(*0.8)) ciopts( lpattern(dash) recast(rcap) msize(small)) msymbol(circle_hollow) msymbol(O) msize(small) mcolor(navy8) addplot(line @b @at,lwidth(sthick) lcolor(navy8)) graphregion(color(white))
graph save "平行趋势检验TBR", replace

reghdfe Theilt密度 eventz* T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id)
coefplot,baselevels keep(event*) vertical coeflabels(eventz1 = "-4" eventz2 = "-3" eventz3 = "-2" eventz4 = "-1" eventz5 = "0" eventz6 = "1" eventz7 = "2" eventz8 = "3" eventz9 = "4") title("Parallel trend test: INEQ", size(*0.8)) yline(0, lc(black*0.5) lp(dash)) xline(4, lc(black*0.5) lp(dash)) ylabel(, nogrid format(%6.2f) labsize(small))  ytitle("Coefficients", size(*0.8)) xtitle("Time passage relative to year of UAP", size(*0.8)) ciopts( lpattern(dash) recast(rcap) msize(small)) msymbol(circle_hollow) msymbol(O) msize(small) mcolor(navy8) addplot(line @b @at,lwidth(sthick) lcolor(navy8)) graphregion(color(white))
graph save "平行趋势检验Theil", replace

graph combine 平行趋势检验TWR.gph 平行趋势检验TBR.gph 平行趋势检验Theil.gph, row(1) graphregion(color(white))

///Placebo Test
clear
forvalues k = 1/1000{
mat b = J(500,1,0)
mat se = J(500,1,0)
mat p = J(500,1,0)
forvalues i = 1/500{
  use "C:\Users\katrina\Desktop\ybyTheil10近邻匹配.dta",clear
  xtset id year
  duplicates drop id, force
  sample 157,count
  keep id
  save matchid.dta,replace
  merge 1:m id using "C:\Users\katrina\Desktop\ybyTheil10近邻匹配.dta"
  gen treat=(_merge == 3)
  save matchid`i'.dta,replace
  
  use "C:\Users\katrina\Desktop\ybyTheil10近邻匹配.dta",clear
  xtset id year
  duplicates drop id, force
  gen year1 = 2011+int(9*runiform())
  keep year1
  save matchyear.dta,replace
  mkmat year, matrix(sampleyear)
  
  use matchid`i'.dta,replace
  xtset id year
  gen time = 0
  foreach j of numlist 1/271 {
  replace time = 1 if (id == `j' &  year >= sampleyear[`j',1])
  }

  gen DIDD=time*treat  
  reghdfe TBRt密度 DIDD T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积,absorb(year id) vce(cluster id) 
  mat b[`i', 1] = _b[DIDD]
  mat se[`i', 1] = _se[DIDD]
  scalar df_r = e(N) - e(df_m) - 1
  mat p[`i', 1] = 2*ttail(df_r,abs(_b[DIDD]/_se[DIDD]))
}
 
svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)

drop if pvalue == .
label var pvalue
label var coef
keep coef se pvalue


twoway (scatter pvalue coef, msymbol(smcircle_hollow) mcolor(navy)) (kdensity coef, yaxis(2)), title("Placebo Test: inter-INEQ", size(*0.8)) xline(-0.0051, lc(black*0.5) lp(dash)) xline(0, lc(black*0.5) lp(solid)) yline(0.05, lc(black*0.5) lp(dash)) xlabel(-0.008(0.004)0.008) xtitle("Coefficients", size(*0.8)) xlabel(, format(%05.3f) labsize(small)) ytitle("P Value", size(*0.8)) ylabel(, nogrid format(%4.1f) labsize(small)) ytitle("Kdensity of Estimates", size(*0.8) axis(2)) ylabel(, nogrid format(%4.1f) labsize(small) axis(2)) legend(r(1) order(1 "P Value" 2 "Kdensity of Estimates")) graphregion(color(white))
graph save "安慰剂检验TBR`k'", replace
forvalue i=1/500{
  erase  matchid`i'.dta 
}
erase  matchid.dta 
erase  matchyear.dta 
}


/////逐年k近邻匹配(k=15)
forvalue i = 2011/2019{
 use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
 keep if year == `i'
 set seed 10000
 gen tmp = runiform()
 sort tmp
 psmatch2 treat T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, neighbor(15) common odds logit ties ate
 save `i'.dta, replace
}

clear all

use 2011.dta, clear

forvalue k =2012/2019{
 append using `k'.dta
}

save ybyTheil15近邻匹配.dta, replace

/////逐年k近邻匹配(k=20)
forvalue i = 2011/2019{
 use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
 keep if year == `i'
 set seed 10000
 gen tmp = runiform()
 sort tmp
 psmatch2 treat T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, neighbor(20) common odds logit ties ate
 save `i'.dta, replace
}

clear all

use 2011.dta, clear

forvalue k =2012/2019{
 append using `k'.dta
}

save ybyTheil20近邻匹配.dta, replace

/////逐年卡尺k近邻匹配(k=10)
use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
keep if year >= 2011
logistic treat T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, coef
predict p
gen q = log((1-p)/p)
sum q

di 0.25*0.6772084 //倾向性得分标准差的四分之一
0.1693021
//
forvalue i = 2011/2019{
 use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
 keep if year == `i'
 set seed 10000
 gen tmp = runiform()
 sort tmp
 psmatch2 treat T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, neighbor(10) cal(0.1693021) common odds logit ties ate
 save `i'.dta, replace
}

clear all

use 2011.dta, clear

forvalue k =2012/2019{
 append using `k'.dta
}

save ybyTheil卡尺10匹配.dta, replace

/////逐年核密度匹配
forvalue i = 2011/2019{
 use "/Users/apple/Desktop/区域经济 论文/代码/纯泰尔指数邻域同省面板数据1.dta", clear
 keep if year == `i'
 set seed 10000
 gen tmp = runiform()
 sort tmp
 psmatch2 treat T区域规模 T城市化率 T产业结构高级化2 T贷款gdp T开放度fdi1 T人均道路面积, kernel common odds logit ties ate
 save `i'.dta, replace
}

clear all

use 2011.dta, clear

forvalue k =2012/2019{
 append using `k'.dta
}

save ybyTheil核匹配.dta, replace

