
DATA wireless;
    INFILE "C:\Users\fanas\OneDrive\Desktop\ASAS Project\New_Wireless_Fixed.txt"
			DLM=' ' MISSOVER DSD FIRSTOBS=1;
      input 
        @1  Acctno        $ 1-14       
        @15 Actdt         : MMDDYY10. 
        @26 Deactdt       : MMDDYY10.  
        @35 DeactReason    $ 36-52      
        @53 GoodCredit     $ 53      
        @62 RatePlan       $ 62      
        @65 DealerType     $ 65-66     
        @74 Age            74-75         
        @80 Province       $ 80-81      
        @85 Sales          DOLLAR8.2;  
    FORMAT Actdt MMDDYY10. Deactdt MMDDYY10. Sales DOLLAR8.2;  
run;

title 'Customer Distribution and Deactivation Analysys';
proc print data=wireless (obs=10);  
run;


*Q1.1.A)Is the Account Number (Acctno) Unique?;
proc sql; 
    create table duplicate_counts as
    select count(*) as Count
    from (
        select Acctno
        from wireless
        group by Acctno
        having count(*) > 1
    );
quit;

data result_message;
    set duplicate_counts; 
    
    if Count = 0 then 
        message = "There are no duplicates. The Acctno is unique.";
    else 
        message = "There are duplicate Acctno.";
run;

title 'Evaluating the Uniqueness of Account Number';
proc print data=result_message noobs;
    var message;
run;

*Q1.1.B) What is the number of accounts activated and deactivated?;
proc sql;
    create table account_status as
    select 
        count(*) as Total_Accounts,   
        count(Deactdt) as Deactivated_Accounts,  
        count(*) - count(Deactdt) as Activated_Accounts 
    from wireless;
quit;
proc print data=account_status (obs=10);run;
proc transpose data=account_status out=account_status_t;
    var Total_Accounts Deactivated_Accounts Activated_Accounts;
run;
proc print data=account_status_t (obs=10);run;

proc sgplot data=account_status_t;
    vbar _name_ / response=col1 datalabel;
    xaxis label="Account Status";
    yaxis label="Number of Accounts";
    title "Comparison of Total, Activated, and Deactivated Accounts";
run;


*Q1.1.C) When are the earliest and latest activation/deactivation dates available?;
proc sql;
    create table date_ranges as
    select 
        min(Actdt) as Earliest_Activation format=date9.,   
        max(Actdt) as Latest_Activation format=date9.,     
        min(Deactdt) as Earliest_Deactivation format=date9.,   
        max(Deactdt) as Latest_Deactivation format=date9.      
    from wireless;
quit;

proc transpose data=date_ranges out=date_ranges_t;
    var Earliest_Activation Latest_Activation Earliest_Deactivation Latest_Deactivation;
run;
proc sgplot data=date_ranges_t;
    hbar _name_ / response=col1 datalabel;
    xaxis label="Dates";
    yaxis label="Event";
    title "Earliest and Latest Activation/Deactivation Dates";
run;


*Q1.1.D) Summary Statistics for Other Variables (e.g., Sales, Age);
proc means data=wireless n nmiss mean std min max;
    var Age Sales; 
run;

proc contents data=wireless;
run;

 
*1.2) What are the age and province distributions of active and deactivated customers?;
DATA customers_status;
   SET wireless;
   IF Deactdt NE . THEN status = 'deactivated';
   ELSE status = 'active';
RUN;

PROC UNIVARIATE DATA=customers_status NORMAL;
   class Status;
   VAR Age;
   TITLE "Normality Test for Activation Dates (Actdt)";
RUN;

PROC NPAR1WAY DATA=customers_status;
   CLASS status; 
   VAR Age;       
   TITLE "Mann-Whitney U Test for Age Distribution Between Active and Deactivated Customers";
RUN;

PROC FREQ DATA=customers_status;
   TABLES Province*status / CHISQ;
   TITLE "Chi-Square Test of Province Distribution by Customer Status";
RUN;

/*1.3) Segment the customers based on age, province, and sales amount:
Sales segment: < $100, $100-$500, $500-$800, $800 and above.
Age segments: < 20, 21-40, 41-60, 60 and above.*/

DATA segmented_customers;
    SET customers_status ; 

    IF missing(Sales) THEN Sales_Segments = 'Missing            ';
    ELSE IF Sales < 100 THEN Sales_Segments = '< $100';
    ELSE IF Sales >= 100 AND Sales <= 500 THEN Sales_Segments = '$100-$500';
    ELSE IF Sales > 500 AND Sales <= 800 THEN Sales_Segments = '$500-$800';
    ELSE IF Sales > 800 THEN Sales_Segments = '$800 and above';
    
  	IF missing(Age) THEN Age_Segments = 'Missing                        '; 
    ELSE IF Age <= 20 THEN Age_Segments  = '< 20';
    ELSE IF Age >= 21 AND Age <= 40 THEN Age_Segments  = '21-40'; 
    ELSE IF Age >= 41 AND Age <= 60 THEN Age_Segments  = '41-60'; 
    ELSE IF Age > 60 THEN Age_Segments  = '60 and above';
RUN;
proc format;
    value $Province
        'AB' = 'A'
        'ON' = 'O'
        'NS' = 'N'
        'BC' = 'B'
        'QC' = 'Q';
run;
title "Customer Segmentation Analysis: Insights Based on Age, Province, and Sales Amount";
proc print data=segmented_customers (obs=10);
FORMAT Province $Province.; 
RUN;

*For further inspection of the data:;
PROC FREQ DATA=segmented_customers;
    TABLES Sales_Segments*Province / CHISQ; 
    TITLE "Bivariate Analysis: Sales Category by Province";
RUN;

PROC SGPLOT DATA=segmented_customers;
    VBAR Sales_Segments / GROUP=Province GROUPDISPLAY=CLUSTER DATALABEL;    
    XAXIS LABEL="Sales Category" values=('< $100' '$100-$500' '$500-$800' '$800 and above' 'Missing');
    YAXIS LABEL="Count of Customers";     
    TITLE "Distribution of Sales Categories by Province";
RUN;

PROC FREQ DATA=segmented_customers;
    TABLES Sales_Segments * Age_Segments / CHISQ;
    TITLE "Bivariate Analysis of Sales Category and Age Category";
RUN;

PROC SGPLOT DATA=segmented_customers;
    VBAR Sales_Segments / GROUP=Age_Segments GROUPDISPLAY=CLUSTER DATALABEL;
    TITLE "Sales Category by Age Category";
    XAXIS LABEL="Sales Category" values=('< $100' '$100-$500' '$500-$800' '$800 and above' 'Missing');
    YAXIS LABEL="Count of Customers"; 
RUN;


*Q1.4)Statistical Analysis;

*1.4.1)Calculate the tenure in days for each account and give its simple statistics.;
DATA account_data;
    SET wireless ;   
    IF missing(Deactdt) THEN 
        tenure_days_Actdt = TODAY() - Actdt; 
    ELSE 
        tenure_days_Deactdt = Deactdt - Actdt; 
RUN;
title "Calculating Customer Tenure";
proc print data=account_data (obs=10);
run;

title 'Summary of Actdt Tenure';
proc means data=account_data  n nmiss mean std min max;
    var tenure_days_Actdt;
run;

title 'Summary of Deactdt Tenure';
proc means data=account_data n nmiss mean std min max;
    var tenure_days_Deactdt;
run;

*1.4.2)Calculate the number of accounts deactivated for each month.;
proc sql;
    create table deactivation_counts as
    select 
        PUT(Deactdt, MONNAME3.) as Deactivation_Month, 
        COUNT(*) as Number_of_Deactivations
    from 
        account_data 
    where 
        Deactdt IS not null 
    group by 
        calculated Deactivation_Month 
    order by 
        calculated Deactivation_Month; 
quit;

proc sgplot data=deactivation_counts;
    vbar Deactivation_Month / response=Number_of_Deactivations datalabel ; 
    title "Number of Deactivations by Month";
    xaxis label="Month" values=('Jan' 'Feb' 'Mar' 'Apr' 'May' 'Jun' 'Jul' 'Aug' 'Sep' 'Oct' 'Nov' 'Dec');; 
    yaxis label="Number of Deactivations"; 
run;


*1.4.3)number of accounts of percent of all for each segment.;
proc sql;
    create table segmented_accounts as
    select 
        status, 
        case 
            when (Deactdt - Actdt < 30) then '< 30 days'
            when (Deactdt - Actdt <= 60) then '31 - 60 days'
            when (Deactdt - Actdt <= 365) then '61 days - 1 year'
            else 'Over 1 year'
        end as tenure_category,        
        count(*) as account_count
    from customers_status
    group by status, tenure_category  
    order by status, tenure_category;  
quit; 

proc sql;
    create table tenure_percentage as
    select 
        tenure_category,
        account_count,
        (account_count / total_count.total) * 100 as percent_of_all
    from segmented_accounts,
         (select sum(account_count) as total from segmented_accounts) as total_count;
quit;
proc print data=tenure_percentage;run;

proc sgplot data=segmented_accounts;
    vbar tenure_category / response=account_count group=status 
        groupdisplay=cluster datalabel; 
    xaxis label="Tenure Category" values=('< 30 days' '31 - 60 days' '61 days - 1 year' 'Over 1 year');
    yaxis label="Number of Accounts"; 
    title "Number of Accounts by Tenure Category and Status"; 
run;


*1.4.4) Test the general association between the tenure segments and “Good Credit” “RatePlan ” and “DealerType.”;
proc sql;
   create table tenure_segments as  
   select *,
          case 
            when (Deactdt - Actdt < 30) then '< 30 days'
            when (Deactdt - Actdt >= 30 and Deactdt - Actdt <= 60) then '31 - 60 days'
            when (Deactdt - Actdt > 60 and Deactdt - Actdt <= 365) then '61 days - 1 year'
            else 'Over 1 year'
          end as tenure_category
   from customers_status;
quit;

proc freq data=tenure_segments;  
   tables tenure_category * GoodCredit / chisq;
   tables tenure_category * RatePlan / chisq;
   tables tenure_category * DealerType / chisq; 
run;

proc sgplot data=tenure_segments;
   vbar tenure_category / group=GoodCredit groupdisplay=cluster datalabel;
   xaxis label="Tenure Category" values=('< 30 days' '31 - 60 days' '61 days - 1 year' 'Over 1 year');
   yaxis label="Number of Accounts";
   title "Tenure Category Grouped by GoodCredit";
run;

proc sgplot data=tenure_segments;
   vbar tenure_category / group=RatePlan groupdisplay=cluster datalabel;
   xaxis label="Tenure Category" values=('< 30 days' '31 - 60 days' '61 days - 1 year' 'Over 1 year');
   yaxis label="Number of Accounts";
   title "Tenure Category Grouped by RatePlan";
run;

proc sgplot data=tenure_segments;
   vbar tenure_category / group=DealerType groupdisplay=cluster datalabel;
   xaxis label="Tenure Category" values=('< 30 days' '31 - 60 days' '61 days - 1 year' 'Over 1 year');
   yaxis label="Number of Accounts";
   title "Tenure Category Grouped by DealerType";
run;


*1.4.5)Is there any association between the account status and the tenure segments?;
/*Could you find a better tenure segmentation strategy that is more associated with the account status?*/

proc freq data=tenure_segments;
   tables tenure_category * status /chisq;
run;

proc sgplot data=tenure_segments;
   vbar tenure_category / group=status groupdisplay=stack;
   xaxis label="Tenure Category" values=('< 30 days' '31 - 60 days' '61 days - 1 year' 'Over 1 year');
   yaxis label="Number of Accounts";
   title "Stacked Bar Chart of Account Status by Tenure Category";
run;

*PART 2;
proc sql;
   create table tenure_segments2 as
   select *,
          case 
            when (Deactdt - Actdt < 20) then '< 20 days'
            when (Deactdt - Actdt >= 20 and Deactdt - Actdt < 40) then '21 - 40 days'
            when (Deactdt - Actdt >= 40 and Deactdt - Actdt < 80) then '41 - 80 days'
            when (Deactdt - Actdt >= 80 and Deactdt - Actdt < 100) then '81 - 100 days'
            when (Deactdt - Actdt >= 100 and Deactdt - Actdt < 120) then '101 - 120 days'
            when (Deactdt - Actdt >= 120 and Deactdt - Actdt < 140) then '121 - 140 days'
            when (Deactdt - Actdt >= 140 and Deactdt - Actdt <= 365) then '141 days - 1 year'
            else 'Over 1 year'
          end as tenure_category
   from customers_status;
quit;
proc print data=tenure_segments2 (obs=10);run;

proc freq data=tenure_segments2;
   tables tenure_category * status /chisq ;
run;
proc sgplot data=tenure_segments2;
   vbar tenure_category / group=status groupdisplay=stack;
   yaxis label="Number of Accounts";
   xaxis label="Tenure Category";
   title "Stacked Bar Chart of Account Status by Tenure Category";
run;


*1.4.6)Does the Sales amount differ among different account statuses, GoodCredit, and customer age segments?;
PROC UNIVARIATE DATA=segmented_customers NORMAL;
   CLASS status; 
   VAR Sales;
   TITLE "Normality Test for Sales Amount by Account Status";
RUN;

PROC NPAR1WAY DATA=segmented_customers;
   CLASS status;
   VAR Sales;
   TITLE "Mann-Whitney U Test for Sales Amount by Account Status";
RUN;


PROC UNIVARIATE DATA=segmented_customers NORMAL;
   CLASS GoodCredit;  
   VAR Sales;
   TITLE "Normality Test for Sales Amount by GoodCredit";
RUN;

PROC NPAR1WAY DATA=segmented_customers;
   CLASS GoodCredit; 
   VAR Sales;       
   TITLE "Mann-Whitney U Test for Sales Amount by Good Credit Status";
RUN;


PROC UNIVARIATE DATA=segmented_customers NORMAL;
   CLASS Age_Segments;  
   VAR Sales;
   TITLE "Normality Test for Sales Amount by Age Segments";
RUN;

PROC NPAR1WAY DATA=segmented_customers;
   CLASS Age_Segments; 
   VAR Sales;       
   TITLE "Kruskal-Wallis H Test for Sales Amount by Age Segments";
RUN;








