SELECT
    r.review_id,
    r.business_id,
    r.user_id,

    r.stars AS review_stars,

    CASE 
        WHEN r.stars >= 4 THEN 1
        ELSE 0
    END AS satisfied,

    r.date AS review_date,
    LENGTH(r.text) AS review_length,
    r.useful,
    r.funny,
    r.cool,

    b.name AS business_name,
    b.city,
    b.state,
    b.latitude,
    b.longitude,
    b.stars AS business_stars,
    b.review_count AS business_review_count,
    b.is_open,
    b.categories,

    CASE
        WHEN (b.attributes::json) ->> 'RestaurantsPriceRange2' IN ('1','2','3','4')
        THEN CAST((b.attributes::json) ->> 'RestaurantsPriceRange2' AS INTEGER)
        ELSE NULL
    END AS business_price,

    CASE
        WHEN STRPOS(COALESCE((b.attributes::json) ->> 'BusinessParking',''),'True') > 0
        THEN 1
        ELSE 0
    END AS has_parking,

    CASE
        WHEN COALESCE((b.attributes::json) ->> 'RestaurantsReservations','False') = 'True'
        THEN 1
        ELSE 0
    END AS takes_reservations,

    CASE
        WHEN COALESCE((b.attributes::json) ->> 'OutdoorSeating','False') = 'True'
        THEN 1
        ELSE 0
    END AS outdoor_seating,

    CASE
        WHEN COALESCE((b.attributes::json) ->> 'HasTV','False') = 'True'
        THEN 1
        ELSE 0
    END AS has_tv,

    u.review_count AS user_review_count,
    u.average_stars AS user_average_stars,
    u.fans AS user_fans,

    CASE 
        WHEN u.elite IS NULL OR u.elite = '' THEN 0
        ELSE 1
    END AS user_is_elite,

    EXTRACT(DOW FROM r.date) AS review_day_of_week,

    CASE 
        WHEN EXTRACT(DOW FROM r.date) IN (0,6) THEN 1
        ELSE 0
    END AS weekend

FROM public3.reviewtable r

JOIN public3.businesstable b
    ON r.business_id = b.business_id

LEFT JOIN public3.userstable u
    ON r.user_id = u.user_id

WHERE b.categories ILIKE '%Restaurants%'
;
