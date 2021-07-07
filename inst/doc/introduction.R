## ---- eval=FALSE--------------------------------------------------------------
#  
#  install.packages("tidyHeatmap")
#  

## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(dplyr)
library(tidyr)
library(tidyHeatmap)

## -----------------------------------------------------------------------------
mtcars_tidy <- 
	mtcars %>% 
	as_tibble(rownames="Car name") %>% 
	
	# Scale
	mutate_at(vars(-`Car name`, -hp, -vs), scale) %>%
	
	# tidyfy
	pivot_longer(cols = -c(`Car name`, hp, vs), names_to = "Property", values_to = "Value")

mtcars_tidy

## -----------------------------------------------------------------------------
mtcars_heatmap <- 
	mtcars_tidy %>% 
		heatmap(`Car name`, Property, Value	) %>%
		add_tile(hp)

mtcars_heatmap

## ----eval=F-------------------------------------------------------------------
#  mtcars_heatmap %>% save_pdf("mtcars_heatmap.pdf")

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	group_by(vs) %>%
	heatmap(`Car name`, Property, Value	) %>%
	add_tile(hp)

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	group_by(vs) %>%
	heatmap(
		`Car name`, Property, Value	,
		palette_grouping = list(c("#66C2A5", "#FC8D62"))
	) %>%
	add_tile(hp)


## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	heatmap(`Car name`, Property, Value	) %>%
	split_rows(2) %>%
	split_columns(2)

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	heatmap(
		`Car name`, Property, Value	,
		row_km = 2,
		column_km = 2
	) 

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	heatmap(
		`Car name`, 
		Property, 
		Value,
		palette_value = c("red", "white", "blue")
	)

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	heatmap(
		`Car name`, 
		Property, 
		Value,
		palette_value = circlize::colorRamp2(
			seq(-2, 2, length.out = 11), 
			RColorBrewer::brewer.pal(11, "RdBu")
		)
	)


## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	heatmap(
		`Car name`, 
		Property, 
		Value,
		palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
	)

## -----------------------------------------------------------------------------
tidyHeatmap::pasilla %>%
	group_by(location, type) %>%
	heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) %>%
	add_tile(condition) %>%
	add_tile(activation)

## -----------------------------------------------------------------------------
# Create some more data points
pasilla_plus <- 
	tidyHeatmap::pasilla %>%
		dplyr::mutate(act = activation) %>% 
		tidyr::nest(data = -sample) %>%
		dplyr::mutate(size = rnorm(n(), 4,0.5)) %>%
		dplyr::mutate(age = runif(n(), 50, 200)) %>%
		tidyr::unnest(data) 

# Plot
pasilla_plus %>%
		heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) %>%
	add_tile(condition) %>%
	add_point(activation) %>%
	add_tile(act) %>%
	add_bar(size) %>%
	add_line(age)

## -----------------------------------------------------------------------------
tidyHeatmap::pasilla %>%
	
	# filter
	filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) %>%
	
	heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) %>% 
	layer_point(
		`count normalised adjusted log` > 6 & sample == "untreated3" 
	)

## -----------------------------------------------------------------------------
	mtcars_tidy %>% 
		heatmap(
			`Car name`, Property, Value, 
			rect_gp = grid::gpar(col = "#161616", lwd = 0.5)
		) 

## -----------------------------------------------------------------------------
	mtcars_tidy %>% 
		heatmap(
			`Car name`, Property, Value, 
			cluster_rows = FALSE
		) 

