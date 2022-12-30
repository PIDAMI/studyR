# ЗАДАНИЕ 19
# Используйте файл Payment_and_value_of_Care-Hospital.csv 
# Напишите функцию, которая на вход принимает название города и название медицинской процедуры/заболевания,
#  а на выход выдает именованный список, в котором в качестве имен используется название больницы в которой можно получить медицинскую помощь, 
#  а качестве значений – усредненная стоимость услуги в данной больнице. 
# Список должен быть упорядочен по возрастанию значения стоимости услуги.

df <- read.csv("Payment_and_value_of_care_-_Hospital.csv",sep=',',allowEscapes=TRUE)

get_disease <- function(display_name) {
  display_name <- gsub("Value of Care ", "", display_name)
  display_name <- gsub(" measure", "", display_name)
  return(display_name)
}
  
payment_to_numerical<-function(payment) {
  if (payment == 'Not Available')
    return(NA)
  payment_formatted = gsub(",",".",gsub("\\$","",payment))
  return(as.numeric(payment_formatted))
}

# INPUT DISEASE VALUE INTO FUNCTION ARGUMENT FROM THIS LIST
diseases = lapply(unique(df['Value.of.care.display.name']$Value.of.care.display.name), get_disease)

avg_cost_to_hospital <- function(df, city, disease){
  que = get_disease(df$Value.of.care.display.name) == disease & df$City == city
  qued_df = df[que, c("Hospital.name", "Payment")]
  cities = unique(qued_df$Hospital.name)
  res = setNames(integer(length(cities)), cities)
  for (city in cities) {
    payments = payment_to_numerical(qued_df[qued_df$Hospital.name == city, "Payment"])
    res[city] = mean(payments)
  }
  return(sort(res, na.last = TRUE))
}

# EXAMPLE
avg_cost_to_hospital(df, "LAS VEGAS", "Heart Failure")
