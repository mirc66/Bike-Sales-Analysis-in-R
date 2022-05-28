
#  first sales analysis----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)




# 2.0 Importing Files ----
#Hit tab from within path and in autocompletes

bikes_tbl<-read_excel(path="00_data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel(path="00_data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel(path="00_data/bike_sales/data_raw/orderlines.xlsx")

# 3.0 Examining Data ----


bikes_tbl# displays it on console gives structure of variable char, num, fact

glimpse(bikes_tbl)

orderlines_tbl

# 4.0 Joining Data ----

?left_join # find info ?help

orderlines_tbl #sends to console
bikes_tbl

left_join(orderlines_tbl,bikes_tbl, by= c("product.id"="bike.id"))

# another way to do the above with %>% pipeline so you can do more than one join

bike_orderline_joined_tbl <-orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id"= "bikeshop.id"))
    
# to see it in glimpse you can use the %>%

bike_orderline_joined_tbl%>% glimpse()
  
# 5.0 Wrangling Data ----

# ?separate() # shows the help on how to format
?ends_with() #select tidyselect helpers 

bike_orderlines_wrangled_tbl <- bike_orderline_joined_tbl %>%
    
       separate(description, #Separate description into category.1, category.2, and frame.material
             into = c("category.1","category.2","frame.material"),
             sep = " - ") %>%
    
        separate(location, #separate location in to city & state
                 into= c("city","state"),
                 sep=", ",
                 remove= FALSE)%>%
        #price extended
        mutate(total.price= price * quantity)%>%
        
        #reorganize  delete ....1 and location columns
        select(-...1,-location)%>%
        select(-ends_with(".id"))%>% # selects all but the .id features
        bind_cols(bike_orderline_joined_tbl %>% select(order.id))%>% #bind vs join dont have a common key column to join by
        
        #reorder colums for easy of understanding first 3 is partial name of column other 3 full column name
       
        select(contains("date"),contains("id"),contains("order"),
               quantity,price,total.price,
               everything())%>%
    
        #rename columns
        rename(order_date=order.date)%>% #renames one column at atime
        #set_names(names(.)%>%toupper())%>%#sets all incoming names to upper case
        set_names(names(.)%>%str_replace_all("\\.","_"))# . needs reg expression to use hence\\ also final save take pipe to glimpse out
    
    
       # glimpse() # shows in console comment out for final save

bike_orderlines_wrangled_tbl%>%glimpse()#display in console with data types

# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl<-bike_orderlines_wrangled_tbl%>%
    
    #Selecting columns to focus on and add a year columns
    select(order_date,total_price)%>%
    mutate(year=year(order_date))%>%
    
    #Grouping by year and summarizing sales
    group_by(year)%>%
    summarize(sales=sum(total_price))%>%
    ungroup() %>%
    
    #for graphs convert sales to text and format with $
    mutate(sales_text=scales::dollar(sales))
    



# Step 2 - Visualize

#?palette_light() #gives colors in this pallet in hex
#scales::show_col(palette_light()) #shows color and hex rep

sales_by_year_tbl%>%
    
    #set up canvas with year in x and sales on y AXIS
    ggplot(aes(x=year,y=sales))+
    
    #geometries
    geom_col(fill="#2C3E50")+
    geom_label(aes(label=sales_text))+
    geom_smooth(method="lm",se=FALSE) + #creates a linear regression se=F takes error lines off
    
    #Formatting 
    theme_tq()+
    scale_y_continuous(labels=scales::dollar)+
    labs(
        title= "Revenue by Year",
        subtitle="Upward trend",
        x="", #blank quotes takes label off
        y="Revenue"
    )
    
# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate
sales_by_year_cat_2_tbl<-bike_orderlines_wrangled_tbl%>%
    
    #selecting columns we want to focus on and add a year column
    select(order_date,total_price,category_2)%>%
    mutate(year=year(order_date))%>%
    
    #group by and summarize year and category2
    group_by(year,category_2)%>%
    summarise(sales=sum(total_price))%>%
    ungroup()%>% #after any group and summary to prevent errors

    #format $ text
    mutate(sales_text=scales::dollar(sales))

sales_by_year_cat_2_tbl
    
    
# Step 2 - Visualize

sales_by_year_cat_2_tbl%>%
    
    #set x and y axis and fill
    ggplot(aes(x=year, y= sales, fill=category_2))+

    #geometries
    geom_col()+
    geom_smooth(method="lm",se=FALSE)+
    
    #facets 
    
    #facet_wrap(~category_2,ncol=3,scales="free")  # scale=free each face has its own x and y axis scales=free
    # facet_wrap(~category_2,ncol=3,scales="free_y") # x label disappears except on bottom line,y scale each different
    facet_wrap(~category_2,ncol=3,scales="free_y") + # all y axis are at same scale

    #Formatting
    theme_tq()+
    scale_fill_tq()+
    scale_y_continuous(labels=scales::dollar)+
    labs(
        title="Revenue by Year and Category2",
        subtitle="Each product category has an upward trend",
        x="",# gets rid of year on x axis
        y="Revenue",
        fill="Product Secondary Category" # in the legend only
    )
    

# 7.0 Writing Files ----

#creates data_wrangled_student folder in the given path
fs::dir_create("00_data/bike_sales/data_wrangled_student")

# 7.1 Excel ----

bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/bike_sales/data_wrangled_student/bike_orderlines.xls")

# 7.2 CSV ----

bike_orderlines_wrangled_tbl %>%
  write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")

# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>%
  write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")
