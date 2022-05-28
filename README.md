# Bike-Sales-Analysis-in-R
The **Bike Sales Database** represents a **bicycle manufacturer**,
including tables for products (bikes), customers (bike shops), and
transactions (orders)
#
The Bike Sales data files consist of 3 excel spreadsheets that
represent:

-   `bikes` table which includes bicycle models, descriptions, and unit
    prices that are produced by the manufacturer.

-   `bikeshops` table which includes customers that the bicycle
    manufacturer has sold to.

-   `orderlines` table which includes transactional data such as order
    ID, order line, date, customer, product, and quantity sold.
#
 ## $bikes
    ## # A tibble: 97 x 4
    ##    bike.id model                          description                price
    ##      <dbl> <chr>                          <chr>                      <dbl>
    ##  1       1 Supersix Evo Black Inc.        Road - Elite Road - Carbon 12790
    ##  2       2 Supersix Evo Hi-Mod Team       Road - Elite Road - Carbon 10660
    ##  3       3 Supersix Evo Hi-Mod Dura Ace 1 Road - Elite Road - Carbon  7990
    ##  4       4 Supersix Evo Hi-Mod Dura Ace 2 Road - Elite Road - Carbon  5330
    ##  5       5 Supersix Evo Hi-Mod Utegra     Road - Elite Road - Carbon  4260
    ##  6       6 Supersix Evo Red               Road - Elite Road - Carbon  3940
    ##  7       7 Supersix Evo Ultegra 3         Road - Elite Road - Carbon  3200
    ##  8       8 Supersix Evo Ultegra 4         Road - Elite Road - Carbon  2660
    ##  9       9 Supersix Evo 105               Road - Elite Road - Carbon  2240
    ## 10      10 Supersix Evo Tiagra            Road - Elite Road - Carbon  1840
    ## # ... with 87 more rows
    ## 
    ## $bikeshops
    ## # A tibble: 30 x 3
    ##    bikeshop.id bikeshop.name                location       
    ##          <dbl> <chr>                        <chr>          
    ##  1           1 Pittsburgh Mountain Machines Pittsburgh, PA 
    ##  2           2 Ithaca Mountain Climbers     Ithaca, NY     
    ##  3           3 Columbus Race Equipment      Columbus, OH   
    ##  4           4 Detroit Cycles               Detroit, MI    
    ##  5           5 Cincinnati Speed             Cincinnati, OH 
    ##  6           6 Louisville Race Equipment    Louisville, KY 
    ##  7           7 Nashville Cruisers           Nashville, TN  
    ##  8           8 Denver Bike Shop             Denver, CO     
    ##  9           9 Minneapolis Bike Shop        Minneapolis, MN
    ## 10          10 Kansas City 29ers            Kansas City, KS
    ## # ... with 20 more rows
    ## 
    ## $orderlines
    ## # A tibble: 15,644 x 7
    ##    ...1  order.id order.line order.date          customer.id product.id quantity
    ##    <chr>    <dbl>      <dbl> <dttm>                    <dbl>      <dbl>    <dbl>
    ##  1 1            1          1 2011-01-07 00:00:00           2         48        1
    ##  2 2            1          2 2011-01-07 00:00:00           2         52        1
    ##  3 3            2          1 2011-01-10 00:00:00          10         76        1
    ##  4 4            2          2 2011-01-10 00:00:00          10         52        1
    ##  5 5            3          1 2011-01-10 00:00:00           6          2        1
    ##  6 6            3          2 2011-01-10 00:00:00           6         50        1
    ##  7 7            3          3 2011-01-10 00:00:00           6          1        1
    ##  8 8            3          4 2011-01-10 00:00:00           6          4        1
    ##  9 9            3          5 2011-01-10 00:00:00           6         34        1
    ## 10 10           4          1 2011-01-11 00:00:00          22         26        1
    ## # ... with 15,634 more rows
    #
    # the output are the two plots :
    #
    #RplotRevenuebyYear.pdf
    #RplotRevenuebyYearCategory.pdf
    #
    #the wrangled data was saved in 3 formats xls, csv, rds
    # same file name with different file formats
    #
    #bike_orderlines.xls
