y
chestclinic.dag <- dag(list(
    "asia"
   ,c("tub", "asia")
   ,"smoke"
   ,c("lung", "smoke")
   ,c("bronc", "smoke")
   ,c("either", "lung", "tub")
   ,c("xray", "either")
   ,c("dysp", "bronc", "either")
    ));

#iplot(chestclinic.dag);
