(StartDate as date,EndDate  as date,  EndDateContract as date , pmt as number, rate as number, Canceled as logical )=>

let
/*
  // variable debugging
     StartDate = #date(2023, 01, 05),
     EndDate = #date(2034, 12, 15),
    EndDateContract = #date(2036, 07, 25),
    DaysToEndContract = 4531,
   // DaysPeriod = 4531,
    pmt = 1572.31,
    rate = 0.004669839,
    Status = true, 
*/   
   // Calculation Days 
    xDaysContract = fntDaysOK( StartDate, EndDateContract),
    xDaysSubPeriod = fntDaysOK( StartDate, EndDate),

    xDaysFirstPeriod = fntDaysOK(StartDate, Date.EndOfMonth(StartDate)),
    xDaysEndFirstPeriod_To_Contract = xDaysContract - xDaysFirstPeriod, 

    xDaysEndPeriod = fntDaysOK(Date.StartOfMonth(EndDate), EndDate),
    xDaysRemaingEndPeriod = xDaysContract- xDaysSubPeriod ,
   
   // Calculation First Period
    LTM_1erAper = fntPV_reduce_Ok(xDaysContract/30,  pmt, rate ),
    LTM_1erCier =fntPV_reduce_Ok( xDaysEndFirstPeriod_To_Contract/30,  pmt, rate ),
    
    Rent1er = pmt*(  xDaysFirstPeriod/30),
    FinCost_1er = LTM_1erCier -  LTM_1erAper  - Rent1er,


    // Calculation Las Period
    LTM_FinalRamaing = if xDaysContract = xDaysSubPeriod then 0 else   fntPV_reduce_Ok( xDaysRemaingEndPeriod/30, pmt, rate ),


      FinCostEndPeriod =   TotalFinancialCost,
    

  // Other Value
   TotalPayment = (pmt/30)*xDaysSubPeriod,
   TotalFinancialCost = -( LTM_1erAper +TotalPayment- LTM_FinalRamaing),






   // Define the number of elements
  nNumOfFullPeriods = (Date.Year(EndDate) -Date.Year(StartDate)) * 12 + (Date.Month(EndDate) - Date.Month(StartDate)) + 1,
  
  ListLength  = if EndDate < Date.FromText(Text.From(Date.Year(EndDate)) & "-" & Text.From(Date.Month(EndDate)) & "-01") 
                         then nNumOfFullPeriods- 1 
                else  nNumOfFullPeriods,


  // Generate the list using List.Generate
    Generated = List.Generate(
        ()=> [
               i = 0,
               Days = xDaysFirstPeriod ,
               DaysAcc =  xDaysContract,
               DateKey = Date.EndOfMonth(StartDate),
               PV_Aper =  LTM_1erAper,
               Rent= Rent1er,
               FinCost =  FinCost_1er,
               CumulativeFinCost = FinCost_1er,
               PV_Cier=   LTM_1erCier
                

             ], 

        each [i] < ListLength,
                    
        each [
              i = [i] + 1,
              Days = if  [i] < ListLength -2 then  30 else    xDaysEndPeriod ,     
              DaysAcc = [DaysAcc] - Days,                                                                  
              DateKey =   Date.EndOfMonth( Date.AddMonths( [DateKey], 1)),
              PV_Aper = if [i] = 0  then LTM_1erCier  else [PV_Cier] ,
              Rent =pmt,
              FinCost = if [i] = 0  then LTM_1erCier *rate   else if i = ListLength - 1   then  PV_Aper * rate*(xDaysEndPeriod/30)   else PV_Aper * rate,
              CumulativeFinCost =  [CumulativeFinCost] + FinCost,
              // if [i] = ListLength - 1 then  [CumulativeFinCost]  + PV_Aper*rate * (xDaysEndPeriod/30)           else  [CumulativeFinCost]  + PV_Aper*rate,    
              
              PV_Cier = PV_Aper + pmt + FinCost

              ],
    
        each [
            i = [i],
            Days = [Days],
           // DaysAcc = [DaysAcc], 
            DateKey = [DateKey], 
            PV_Aper =Number.Round( [PV_Aper],4),
            Rent = 
                         if [i] = 0 then pmt * ([Days] / 30) 
                         else if  fntIsEndContrat([DateKey], EndDateContract) then -([PV_Aper]+[FinCost]) 
                         else if [i] = ListLength - 1 then pmt * ([Days] / 30) 
                         else pmt,

            FinCost =   if [i] = ListLength - 1 then  [FinCost]-  ( [CumulativeFinCost]- TotalFinancialCost)    
                        else  Number.Round([FinCost] ,4) ,
    
            PV_Cier =   if fntIsEndContrat([DateKey], EndDateContract)  then  0 
                        else Number.Round([PV_Cier],4),
            
            GainLoss = null    


        ]
                       
    ),

  Totbl  =    Table.FromRecords( Generated)
in  
  Totbl
