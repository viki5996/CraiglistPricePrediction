/* Importing the required dataset */
proc import out= WORK.craig 
     datafile= "H:\craig1.csv" 
     dbms=CSV REPLACE;
     getnames=YES;
     datarow=2; 
run;

proc contents data=craig;
run;

/* Convert character to numeric type */
data craig1;
 set craig;
 odometer_new=input(odometer,best12.);
 price_new=input(price,best12.);
 weather_new=input(weather,best12.);
 year_new=input(year,best12.);
run;

proc contents data=craig1;
run;


/* Cleaning up price variable */
data craig_p1;
	set craig1;
	If price_new < 50 then delete;
run;

data craig_p2;
    set craig_p1;
	If price_new > 100000 then delete;
run;

/* Imputing Mean Values for Price */
proc stdize data=craig_p2 reponly method=mean out=craig_p1;
var price_new;
run;

/* Cleaning up odometer variable */
data craig_o1;
	set craig_p2;
	if odometer_new > 500000 then delete;
run;

/* Cleaning up year variable */
data craig_year;
	set craig_o1;
	If year_new <= 1900 then delete;
run;

data craig_year1;
	set craig_year;
	If year_new => 2020 then delete;
run;


/* Filling up missing values of categorical variables */
data craig_cat;
	set craig_year;
	If state_name = "FAILED" then state_name="unkown";
	If state_name = " " then state_name="unkown";
	If manufacturer = " " then manufacturer="other";
	If transmission = " " then transmission="other";
	If cylinders = " " then cylinders="unkown";
	If condition = " " then condition="unkown";
	If size = " " then size="unkown";
	If type = " " then type="unkown";
	If paint_color = " " then paint_color="unkown";
	If drive = " " then drive="NA";
run;

/* Generating Dummy variables for required columns */
DATA craig_dummy1;
      SET craig_cat ;
IF fuel = "gas" THEN fuel_gas = 1; 
        ELSE fuel_gas = 0;
IF fuel = "hybrid" THEN fuel_hybrid = 1; 
        ELSE fuel_hybrid = 0;
IF fuel = "diesel" THEN fuel_diesel = 1; 
        ELSE fuel_diesel = 0;
IF transmission = "automatic" THEN transmission_automatic = 1; 
        ELSE transmission_automatic = 0;
IF transmission = "manual" THEN transmission_manual = 1; 
        ELSE transmission_manual = 0;
IF size = "full-size" THEN size_fullsize = 1; 
        ELSE size_fullsize = 0;
IF size = "mid-size" THEN size_midsize = 1; 
        ELSE size_midsize = 0;
IF size = "compact" THEN size_compact = 1; 
        ELSE size_compact = 0;
IF size = "sub-compact" THEN size_subcompact = 1; 
        ELSE size_subcompact = 0;
IF size = "unkown" THEN size_unkown = 1; 
        ELSE size_unkown = 0;
IF cylinders = "6 cylinders" THEN cylinders_6 = 1; 
        ELSE cylinders_6 = 0;
IF cylinders = "4 cylinders" THEN cylinders_4 = 1; 
        ELSE cylinders_4 = 0;
IF cylinders = "8 cylinders" THEN cylinders_8 = 1; 
        ELSE cylinders_8 = 0;
IF cylinders = "other" THEN cylinders_other = 1; 
        ELSE cylinders_other = 0;
IF cylinders = "unkown" THEN cylinders_other = 1; 
        ELSE cylinders_other = 0;
IF condition = "excellent" THEN condition_excellent = 1; 
        ELSE condition_excellent = 0;
IF condition = "good" THEN condition_good = 1; 
        ELSE condition_good = 0;
IF condition = "like new" THEN condition_likenew = 1; 
        ELSE condition_likenew = 0;
IF condition = "fair" THEN condition_fair = 1; 
        ELSE condition_fair = 0;
IF condition = "unkown" THEN condition_unkown = 1; 
        ELSE condition_unkown = 0;
IF drive = "4wd" THEN drive_4wd = 1; 
        ELSE drive_4wd = 0;
IF drive = "rwd" THEN drive_rwd = 1; 
        ELSE drive_rwd = 0;
IF drive = "fwd" THEN drive_fwd = 1; 
        ELSE drive_fwd = 0;
IF drive = "NA" THEN drive_na = 1; 
        ELSE drive_na = 0;
IF title_status = "clean" THEN title_clean = 1; 
        ELSE title_clean = 0;
IF title_status = "rebuilt" THEN title_other = 1; 
        ELSE title_other = 0;
IF title_status = "salvage" THEN title_other = 1; 
        ELSE title_other = 0;
IF title_status = "lien" THEN title_other = 1; 
        ELSE title_other = 0;
RUN;

/* Running the MLR model */

/* Generating logPrice variable */
data craig_lprice1;
    set craig_dummy1;
	logPrice = log(price_new);
run;

proc reg data=craig_lprice1;
model price_new = odometer_new year_new fuel_gas fuel_hybrid fuel_diesel transmission_automatic transmission_manual size_fullsize size_midsize size_compact size_subcompact cylinders_other  cylinders_6 cylinders_4 cylinders_8 condition_excellent condition_likenew condition_fair condition_good condition_unkown drive_rwd drive_fwd drive_4wd title_clean title_other;
	  title "MLR model for Price Prediction with Price as response variable";
run;
quit;


proc reg data=craig_lprice1;
model logPrice = odometer_new year_new fuel_gas fuel_hybrid fuel_diesel transmission_automatic transmission_manual size_fullsize size_midsize size_compact size_subcompact cylinders_other  cylinders_6 cylinders_4 cylinders_8 condition_excellent condition_likenew condition_fair condition_good condition_unkown drive_rwd drive_fwd drive_4wd title_clean title_other;
	  title "MLR model for Price Prediction with logPrice as response variable";
run;
quit;

proc glmselect data=craig_lprice1 plots=all;
   STEPWISE: model logPrice = odometer_new year_new fuel_gas fuel_hybrid fuel_diesel transmission_automatic transmission_manual size_fullsize size_midsize size_compact size_subcompact cylinders_other cylinders_6 cylinders_4 cylinders_8 condition_excellent condition_likenew condition_fair condition_good condition_unkown drive_rwd drive_fwd drive_4wd title_clean title_other/ selection=stepwise 
                   details=steps select=SL slentry=0.05 slstay=0.05;
   title "Stepwise Model Selection for Price with logPrice as response variable";
	run;
	quit;

/* Exploratory Data Analysis */
/* EDA for Price */
proc sgplot data=craig_lprice;
  title "Relationship between logPrice and Odometer";
  scatter y=logPrice x=odometer_new;
run;

proc univariate data=craig_lprice plots;
	histogram price_new logPrice / normal kernel; 
	inset n mean std / position = ne;
	probplot logPrice;
	var logPrice price_new;
run;

/*EDA for Year*/

proc univariate data=craig_lprice plots;
    title " Exploring Year variable ";
    histogram year_new;
	var year_new;
run;

*Distribution of average price every 25 years;
proc sql;
  create table avg_price as 
  select
    case 
      when input(year,?4.) between 1900 and 1924 then '1900 to 1924'
      when input(year,?4.) between 1925 and 1949 then '1925 to 1949'
      when input(year,?4.) between 1950 and 1974 then '1950 to 1974'
      when input(year,?4.) between 1975 and 1999 then '1975 to 1999'
      when input(year,?4.) between 2000 and 2017 then '2000 to 2017'
      else 'out of range'
    end
    as years
  , avg(price_new) as average_price
  from work.craig_lprice
  group by years
  having years not in ('out of range')
;
quit;

proc gchart data=avg_price;                                                                                                                
   vbar years / sumvar=average_price maxis=axis1;
   title "Average Price for every 25 years";
run; 



*Distribution of number of cars every 25 years;
proc sql;
  create table count_cars as 
  select
    case 
      when input(year,?4.) between 1900 and 1924 then '1900 to 1924'
      when input(year,?4.) between 1925 and 1949 then '1925 to 1949'
      when input(year,?4.) between 1950 and 1974 then '1950 to 1974'
      when input(year,?4.) between 1975 and 1999 then '1975 to 1999'
      when input(year,?4.) between 2000 and 2025 then '2000 to 2025'
      else 'out of range'
    end
    as years,
	count(*) as rows_count
  from work.craig_lprice
  group by years
;
quit;

proc gchart data=count_cars;                                                                                                                
   vbar years / sumvar=rows_count maxis=axis1;
   title "Average Number of Cars for every 25 years";
run; 

*EDA for top 40 years;
ods graphics on / width=12in height=8in;
proc freq data=craig_lprice;
	tables year_new/
	plots(only)=freqplot(scale=freq);
	where year_new in (1980:2020);
run;


/* EDA for Odometer */
proc univariate data=craig_lprice plots;
    title " Exploring Odometer Values ";
    histogram odometer_new;
	var odometer_new;
run;


/* EDA for categorical variables */
proc freq data=craig_lprice order=freq;
	tables city manufacturer condition cylinders fuel title_status transmission drive size type paint_color state_name/
	plots(only)=freqplot(scale=percent);
run;

*Pie chart for manufacturers;
proc gchart data=craig_lprice;
   pie manufacturer/ value=outside percent=outside;
   title "Exploring more of manufacturers";
run;

*Pie chart for condition;
proc gchart data=craig_lprice;
   pie condition/ value=outside percent=outside;
   title "Exploring more of manufacturers";
run;

*Graphs for state_name;
ods graphics on / width=12in height=8in;
proc freq data=craig_lprice order=freq;
	tables state_name/
	plots(only)=freqplot(scale=percent);
run;



/* EDA for weather */
proc univariate data=craig_dummy plots;
    title " Exploring Weather variable ";
    histogram weather_new;
	var weather_new;
run;


/*Logistic Regression*/
ods graphics on / width=6in height=6in;
/* Condition compared to Price */
proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_likenew(event='1') = price_new;
    title 'Car Safety Model';
run;

proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_excellent(event='1') = price_new;
    title 'Car Safety Model';
run;

proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_good(event='1') = price_new;
    title 'Car Safety Model';
run;

proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_fair(event='1') = price_new;
    title 'Car Safety Model';
run;

/* Condition compared to Odometer */
proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_likenew(event='1') = odometer_new;
    title 'Car Safety Model';
run;

proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_excellent(event='1') = odometer_new;
    title 'Car Safety Model';
run;

proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_good(event='1') = odometer_new;
    title 'Car Safety Model';
run;

proc logistic data=craig_lprice
    plots(only)=(effect (clband showobs));
    MLogit2: model condition_fair(event='1') = odometer_new;
    title 'Car Safety Model';
run;



