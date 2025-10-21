# Write the data frame to a CSV file without row names
write.csv(df_kmeans, "quarto/datos/df_kmeans_cluster.csv", row.names = FALSE)
write.csv(resumen_cluster, "quarto/datos/resumen_cluster.csv", row.names = FALSE)
write.csv(genero_slep, "quarto/datos/genero_slep.csv", row.names = FALSE)
write.csv(matricula_time_slep, "quarto/datos/matricula_time_slep.csv", row.names = FALSE)
write.csv(simce_long, "quarto/datos/simce_long.csv", row.names = FALSE)
write.csv(muestra, "quarto/datos/muestra.csv", row.names = FALSE)
write.csv(matricula_final, "quarto/datos/matricula_final.csv", row.names = FALSE)