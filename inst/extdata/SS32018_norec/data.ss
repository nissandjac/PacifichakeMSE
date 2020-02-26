#C 2018 Hake data file with baseline ageing error (cohort-specific ageing error removed)
1966 #_StartYr
2017 #_EndYr
1 #_Nseas
 12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
1 #_spawn_month
1 #_Ngenders
20 #_Nages=accumulator age
1 #_Nareas
2 #_Nfleets (including surveys)
#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=ignore 
#_survey_timing: -1=for use of catch-at-age to override the month value associated with a datum 
#_fleet_area:  area the fleet/survey operates in 
#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)
#_catch_mult: 0=no; 1=yes
#_rows are fleets
#_fleet_type timing area units need_catch_mult fleetname
  1          -1     1    1     0               Fishery  # 1
  3          0.5    1    2     0               Acoustic_Survey  # 2
#_Catch data: yr, seas, fleet, catch, catch_se
#_catch_se:  standard error of log(catch)
#_NOTE:  catch data is ignored for survey fleets

#Year Seas Fleet Catch   Catch_SE
-999  1    1          0  0.01 # equilibrium catch prior to initial year
#
1966  1    1     137700  0.01
1967  1    1     214370  0.01
1968  1    1     122180  0.01
1969  1    1     180130  0.01
1970  1    1     234590  0.01
1971  1    1     154620  0.01
1972  1    1     117540  0.01
1973  1    1     162640  0.01
1974  1    1     211260  0.01
1975  1    1     221350  0.01
1976  1    1     237520  0.01
1977  1    1     132690  0.01
1978  1    1     103637  0.01
1979  1    1     137110  0.01
1980  1    1      89930  0.01
1981  1    1     139120  0.01
1982  1    1     107741  0.01
1983  1    1     113931  0.01
1984  1    1     138492  0.01
1985  1    1     110399  0.01
1986  1    1     210616  0.01
1987  1    1     234148  0.01
1988  1    1     248840  0.01
1989  1    1     298079  0.01
1990  1    1     261286  0.01
1991  1    1     319705  0.01
1992  1    1     299650  0.01
1993  1    1     198905  0.01
1994  1    1     362407  0.01
1995  1    1     249495  0.01
1996  1    1     306299  0.01
1997  1    1     325147  0.01
1998  1    1     320722  0.01
1999  1    1     311887  0.01
2000  1    1     228777  0.01
2001  1    1     227525  0.01
2002  1    1     180697  0.01
2003  1    1     205162  0.01
2004  1    1     342307  0.01
2005  1    1     363135  0.01
2006  1    1     361699  0.01
2007  1    1     293389  0.01
2008  1    1     321802  0.01
2009  1    1     177171  0.01
2010  1    1     230755  0.01
2011  1    1     291670  0.01
2012  1    1     205787  0.01
2013  1    1     285591  0.01
2014  1    1     298705  0.01
2015  1    1     190663  0.01
2016  1    1     329427  0.01
2017  1    1     440380  0.01 
#
-9999 0 0 0 0 # end input of catch data
#
 #_CPUE_and_surveyabundance_observations
#_Units:  0=numbers; 1=biomass; 2=F; >=30 for special types
#_Errtype:  -1=normal; 0=lognormal; >0=T
#_SD_Report: 0=no sdreport; 1=enable sdreport
#_Fleet Units Errtype SD_Report
1 1 0 0 # Fishery
2 1 0 0 # Acoustic_Survey
# Year  month   fleet   obs       se(log)
1995    7       2       1318035   0.0893
1996    7       -2      1         1      # dummy observation
1997    7       -2      1         1      # dummy observation
1998    7       2       1534604   0.0526
1999    7       -2      1         1      # dummy observation
2000    7       -2      1         1      # dummy observation
2001    7       2       861744    0.1059
2002    7       -2      1         1      # dummy observation
2003    7       2       2137528   0.0642
2004    7       -2      1         1      # dummy observation
2005    7       2       1376099   0.0638
2006    7       -2      1         1      # dummy observation
2007    7       2       942721    0.0766
2008    7       -2      1         1      # dummy observation
2009    7       2       1502273   0.0995
2010    7       -2      1         1      # dummy observation
2011    7       2       674617    0.1177
2012    7       2       1279421   0.0673
2013    7       2       1929235   0.0646
2014    7       -2      1         1      # dummy observation
2015    7       2       2155853   0.0829 # note: "revised in early 2016 from 0.092 to 0.0829"
2016    7       -2      1         1      # dummy observation
2017    7       2       1417811   0.0632

-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV
# note, only have units and errtype for fleets with discard 
#_Fleet units errtype
# -9999 0 0 0.0 0.0 # terminator for discard data 
#
0 #_use meanbodysize_data (0/1)
#_COND_30 #_DF_for_meanbodysize_T-distribution_like
# note:  use positive partition value for mean body wt, negative partition for mean body length 
#_yr month fleet part obs stderr
#  -9999 0 0 0 0 0 # terminator for mean body size data 
#
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
2 # binwidth for population size comp 
10 # minimum size in the population (lower edge of first bin and size at age 0.00) 
70 # maximum size in the population (lower edge of last bin) 
1 # use length composition data (0/1)
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet
#_Comp_Error2:  parm number  for dirichlet
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#_mintailcomp addtocomp combM+F CompressBns CompError ParmSelect minsamplesize   
-1            0.001     0       0           0         0          0.001 #_fleet:1_Fishery
-1            0.001     0       0           0         0          0.001 #_fleet:2_Acoustic_Survey
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
26 #_N_LengthBins; then enter lower edge of each length bin
 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70
#_yr month fleet sex part Nsamp datavector(female-male)
-9999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#
15 #_N_age_bins
 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
1 #_N_ageerror_definitions
#age0      age1       age2       age3       age4       age5       age6       age7       age8       age9       age10      age11      age12      age13      age14      age15      age16      age17      age18      age19      age20      yr         def        comment
0.5        1.5        2.5        3.5        4.5        5.5        6.5        7.5        8.5        9.5        10.5       11.5       12.5       13.5       14.5       15.5       16.5       17.5       18.5       19.5       20.5       # Expected ages
0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    0.00001    # SD of observed age

#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet
#_Comp_Error2:  parm number  for dirichlet
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#_mintailcomp addtocomp combM+F CompressBns CompError ParmSelect minsamplesize   
-1            0.001     0       0           1         1          0.001 #_fleet:1_Fishery
-1            0.001     0       0           1         2          0.001 #_fleet:2_Acoustic_Survey
1 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained


# Acoustic survey ages
#year Month  Fleet Sex Partition AgeErr LbinLo LbinHi nTrips  a1      a2      a3      a4      a5      a6      a7      a8      a9      a10     a11     a12     a13     a14     a15
1995  7      2     0   0         1      -1     -1     69      0       20.48   3.26    1.06    19.33   1.03    4.03    16.37   1.44    0.72    24.86   0.24    1.67    0.21    5.32
1998  7      2     0   0         1      -1     -1     105     0       6.83    8.03    17.03   17.25   1.77    11.37   10.79   1.73    4.19    7.60    1.27    0.34    9.74    2.06
2001  7      2     0   0         1      -1     -1     57      0       50.62   10.95   15.12   7.86    3.64    3.84    2.60    1.30    1.34    0.65    0.68    0.87    0.15    0.39
2003  7      2     0   0         1      -1     -1     71      0       23.06   1.63    43.40   13.07   2.71    5.14    3.43    1.82    2.44    1.44    0.49    0.43    0.42    0.52
2005  7      2     0   0         1      -1     -1     47      0       19.07   1.23    5.10    4.78    50.67   6.99    2.50    3.99    2.45    1.71    0.74    0.48    0.14    0.16
2007  7      2     0   0         1      -1     -1     69      0       28.29   2.16    11.64   1.38    5.01    3.25    38.64   3.92    1.94    1.70    0.83    0.77    0.34    0.12
2009  7      2     0   0         1      -1     -1     72      0       0.55    29.33   40.21   2.29    8.22    1.25    1.79    1.93    8.32    3.63    1.44    0.28    0.48    0.26
2011  7      2     0   0         1      -1     -1     46      0       27.62   56.32   3.71    2.64    2.94    0.70    0.78    0.38    0.66    0.97    2.10    0.76    0.31    0.11
2012  7      2     0   0         1      -1     -1     94      0       62.12   9.78    16.70   2.26    2.92    1.94    1.01    0.50    0.23    0.27    0.66    0.98    0.51    0.12
2013  7      2     0   0         1      -1     -1     67      0       2.17    74.97   5.63    8.68    0.95    2.20    2.59    0.71    0.35    0.10    0.13    0.36    0.77    0.38
2015  7      2     0   0         1      -1     -1     78      0       7.45    9.19    4.38    58.98   4.88    7.53    1.69    1.68    1.64    0.95    0.16    0.29    0.24    0.92
2017  7      2     0   0         1      -1     -1     59      0       0.49    52.73   2.80    3.70    3.31    26.02   4.13    2.91    1.14    0.91    0.87    0.42    0.33    0.25

#Aggregate marginal fishery age comps
#year Month Fleet       Sex  Partition  AgeErr LbinLo  LbinHi  nTrips   a1      a2      a3      a4      a5      a6      a7      a8      a9      a10     a11     a12     a13     a14     a15
1975	7	1	0	0	1       -1	-1	13	4.608	33.846	7.432	1.248	25.397	5.546	8.031	10.537	0.953	0.603	0.871	0.451	0.000	0.476	0.000
1976	7	1	0	0	1       -1	-1	142	0.085	1.337	14.474	6.742	4.097	24.582	9.766	8.899	12.099	5.431	4.303	4.075	1.068	2.355	0.687
1977	7	1	0	0	1       -1	-1	320	0.000	8.448	3.683	27.473	3.594	9.106	22.682	7.599	6.544	4.016	3.550	2.308	0.572	0.308	0.119
1978	7	1	0	0	1       -1	-1	341	0.472	1.110	6.511	6.310	26.416	6.091	8.868	21.505	9.776	4.711	4.680	2.339	0.522	0.353	0.337
1979	7	1	0	0	1       -1	-1	116	0.000	6.492	10.241	9.382	5.721	17.666	10.256	17.370	12.762	4.180	2.876	0.963	1.645	0.000	0.445
1980	7	1	0	0	1       -1	-1	221	0.148	0.544	30.087	1.855	4.488	8.166	11.227	5.012	8.941	11.075	9.460	2.628	3.785	1.516	1.068
1981	7	1	0	0	1       -1	-1	154	19.492	4.031	1.403	26.726	3.901	5.547	3.376	14.675	3.769	3.195	10.186	2.313	0.504	0.163	0.720
1982	7	1	0	0	1 	-1	-1	170	0.000	32.050	3.521	0.486	27.347	1.526	3.680	3.894	11.764	3.268	3.611	7.645	0.241	0.302	0.664
1983	7	1	0	0	1 	-1	-1	117	0.000	0.000	34.144	3.997	1.825	23.458	5.126	5.647	5.300	9.383	3.910	3.128	2.259	1.130	0.695
1984	7	1	0	0	1 	-1	-1	123	0.000	0.000	1.393	61.904	3.625	3.849	16.778	2.853	1.509	1.239	3.342	0.923	0.586	1.439	0.561
1985	7	1	0	0	1 	-1	-1	57	0.925	0.111	0.348	7.241	66.754	8.407	5.605	7.106	2.042	0.530	0.654	0.246	0.000	0.000	0.032
1986	7	1	0	0	1 	-1	-1	120	0.000	15.341	5.384	0.527	0.761	43.638	6.898	8.154	8.260	2.189	2.817	1.834	3.133	0.457	0.609
1987	7	1	0	0	1 	-1	-1	56	0.000	0.000	29.583	2.904	0.135	1.013	53.260	0.404	1.250	7.091	0.000	0.744	1.859	1.757	0.000
1988	7	1	0	0	1 	-1	-1	84	0.000	0.657	0.065	32.348	0.980	1.451	0.656	45.959	1.343	0.835	10.498	0.791	0.054	0.064	4.301
1989	7	1	0	0	1 	-1	-1	80	0.000	5.616	2.431	0.288	50.206	1.257	0.292	0.084	35.192	1.802	0.395	2.316	0.084	0.000	0.037
1990	7	1	0	0	1 	-1	-1	163	0.000	5.194	20.559	1.885	0.592	31.349	0.512	0.200	0.043	31.901	0.296	0.067	6.411	0.000	0.992
1991	7	1	0	0	1 	-1	-1	160	0.000	3.464	20.372	19.632	2.522	0.790	28.260	1.177	0.145	0.181	18.688	0.423	0.000	3.606	0.741
1992	7	1	0	0	1 	-1	-1	243	0.461	4.238	4.304	13.052	18.594	2.272	1.044	33.927	0.767	0.078	0.340	18.049	0.413	0.037	2.426
1993	7	1	0	0	1 	-1	-1	172	0.000	1.051	23.240	3.260	12.980	15.666	1.500	0.810	27.421	0.674	0.089	0.120	12.004	0.054	1.129
1994	7	1	0	0	1 	-1	-1	235	0.000	0.037	2.832	21.390	1.265	12.628	18.687	1.571	0.573	29.906	0.262	0.282	0.022	9.634	0.909
1995	7	1	0	0	1 	-1	-1	147	0.619	1.281	0.467	6.309	28.973	1.152	8.051	20.271	1.576	0.222	22.422	0.435	0.451	0.037	7.734
1996	7	1	0	0	1 	-1	-1	186	0.000	18.282	16.242	1.506	7.743	18.140	1.002	4.908	10.981	0.576	0.347	15.716	0.009	0.108	4.439
1997	7	1	0	0	1 	-1	-1	220	0.000	0.737	29.476	24.952	1.468	7.838	12.488	1.798	3.977	6.671	1.284	0.216	6.079	0.733	2.282
1998	7	1	0	0	1 	-1	-1	243	0.015	4.786	20.351	20.288	26.596	2.869	5.399	9.310	0.917	1.557	3.899	0.352	0.092	2.940	0.627
1999	7	1	0	0	1 	-1	-1	509	0.062	10.245	20.364	17.981	20.061	13.198	2.688	3.930	4.008	0.989	1.542	2.140	0.392	0.334	2.066
2000	7	1	0	0	1 	-1	-1	530	0.996	4.218	10.935	14.285	12.880	21.063	13.115	6.548	4.648	2.509	2.070	2.306	1.292	0.720	2.414
2001	7	1	0	0	1 	-1	-1	540	0.000	17.338	16.247	14.250	15.685	8.559	12.100	5.989	1.778	2.232	1.810	0.698	1.421	0.685	1.209
2002	7	1	0	0	1 	-1	-1	449	0.000	0.033	50.642	14.934	9.687	5.719	4.438	6.580	3.546	0.871	0.845	1.036	0.242	0.475	0.953
2003	7	1	0	0	1 	-1	-1	456	0.000	0.105	1.397	67.898	11.643	3.339	4.987	3.191	3.136	2.106	0.874	0.435	0.533	0.125	0.231
2004	7	1	0	0	1 	-1	-1	501	0.000	0.022	5.310	6.067	68.288	8.152	2.187	4.155	2.512	1.281	1.079	0.350	0.268	0.160	0.170
2005	7	1	0	0	1 	-1	-1	613	0.018	0.569	0.464	6.562	5.381	68.724	7.953	2.358	2.909	2.207	1.177	1.090	0.250	0.090	0.248
2006	7	1	0	0	1 	-1	-1	720	0.326	2.808	10.444	1.673	8.567	4.879	59.039	5.275	1.715	2.376	1.133	1.015	0.426	0.135	0.188
2007	7	1	0	0	1 	-1	-1	629	0.760	11.292	3.731	15.451	1.594	6.852	3.836	44.123	5.186	1.721	2.286	1.781	0.506	0.187	0.693
2008	7	1	0	0	1 	-1	-1	794	0.758	9.855	30.597	2.403	14.421	1.027	3.628	3.165	28.005	3.037	1.142	0.731	0.491	0.313	0.428
2009	7	1	0	0	1 	-1	-1	686	0.637	0.519	30.633	27.553	3.355	10.702	1.305	2.258	2.289	16.187	2.484	0.866	0.591	0.281	0.340
2010	7	1	0	0	1 	-1	-1	874	0.028	25.291	3.351	34.805	21.560	2.368	3.010	0.445	0.579	0.975	6.088	0.930	0.309	0.104	0.157
2011	7	1	0	0	1 	-1	-1	1081	2.637	8.499	70.841	2.650	6.417	4.449	1.146	0.820	0.294	0.391	0.118	1.348	0.171	0.110	0.108
2012	7	1	0	0	1 	-1	-1	851	0.181	40.946	11.557	32.994	2.489	5.085	2.516	1.132	0.659	0.231	0.329	0.347	0.869	0.283	0.383
2013	7	1	0	0	1 	-1	-1	1094	0.030	0.544	70.309	5.906	10.474	1.123	3.414	2.059	0.906	1.366	0.264	0.332	0.530	2.280	0.462
2014	7	1	0	0	1 	-1	-1	1130	0.000	3.314	3.731	64.297	6.927	12.170	1.587	3.141	1.827	0.822	0.466	0.118	0.191	0.279	1.131
2015	7	1	0	0	1 	-1	-1	798	3.591	1.136	6.883	3.946	70.024	4.940	5.089	0.958	1.550	1.087	0.202	0.205	0.061	0.054	0.273
2016	7	1	0	0	1 	-1	-1	1426	0.300	49.629	1.690	4.581	2.517	33.171	2.853	3.234	0.804	0.459	0.366	0.213	0.071	0.039	0.073
2017	7	1	0	0	1 	-1	-1	762	3.039	0.534	32.691	2.450	4.259	3.810	40.489	5.603	3.438	1.594	0.769	0.752	0.242	0.083	0.246
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_Use_MeanSize-at-Age_obs (0/1)
#
0 #_N_environ_variables
#Yr Variable Value
#
0 # N sizefreq methods to read 
#
0 # do tags (0/1)
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#  yr, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
# Yr, Seas, Fleet,  Age/Size,  Bin,  selex_prior,  prior_sd
# feature not yet implemented
#
999
