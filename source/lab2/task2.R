# ЗАДАНИЕ 7
# Используйте файл ExpImp.Rdata Напишите функцию, которая по названию субъекта федерации
# выводит значения суммарного экспорта и суммарного импорта по этому субъекту федерации, 
# а также федеральный округ, к которому этот субъект федерации относится.

load("data/ExpImp.RData")

df <- ExpImp

subject_to_district <- function(df, subject) {
  districts = df[grep("федеральный округ", df$Регион), ]
  districts_ind = as.numeric(rownames(k))
  subj_ind = as.numeric(rownames(df[df$Регион == subject, ]))
  if (subj_ind %in% districts_ind | subj_ind == 1) {
    return(subject)
  }
  n = length(districts_ind)
  if (subj_ind > districts_ind[[n]])
    return(df[districts_ind[[n]],"Регион"])
  
  for (ind in 1:(n - 1)) {
    if (subj_ind > districts_ind[[ind]] & subj_ind < districts_ind[[ind + 1]])
      return(df[districts_ind[ind],"Регион"])
  }
}

subject_saldo_district <- function(df, subject) {
  res = list(total_export = 0,
             total_import = 0,
             district = '')
  res["total_export"] = sum(as.numeric(df[df$Регион == subject,grep("Экспорт", names(df))]))
  res["total_import"] = sum(as.numeric(df[df$Регион == subject,grep("Импорт", names(df))]))
  res["district"] = subject_to_district(df, subject)
  return(res)
}

# EXAMPLE
subject_saldo_district(ExpImp, "Республика Дагестан")



