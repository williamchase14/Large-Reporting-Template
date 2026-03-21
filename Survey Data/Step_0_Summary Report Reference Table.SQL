WITH
hd_latest AS (
  SELECT *
  FROM (
    SELECT
      hd.*,
      ROW_NUMBER() OVER (
        PARTITION BY hd.unitid
        ORDER BY hd.year DESC NULLS LAST
      ) AS rn
    FROM institutional_characteristics_hd hd
  ) x
  WHERE rn = 1
),
state_map(stabbr, state_name, Region) AS (
  VALUES
    ('AL','Alabama','South'), ('AK','Alaska','West'), ('AZ','Arizona','West'),
    ('AR','Arkansas','South'), ('CA','California','West'), ('CO','Colorado','West'),
    ('CT','Connecticut','Northeast'), ('DE','Delaware','South'), ('DC','District of Columbia','South'),
    ('FL','Florida','South'), ('GA','Georgia','South'), ('HI','Hawaii','West'),
    ('ID','Idaho','West'), ('IL','Illinois','Midwest'), ('IN','Indiana','Midwest'),
    ('IA','Iowa','Midwest'), ('KS','Kansas','Midwest'), ('KY','Kentucky','South'),
    ('LA','Louisiana','South'), ('ME','Maine','Northeast'), ('MD','Maryland','South'),
    ('MA','Massachusetts','Northeast'), ('MI','Michigan','Midwest'), ('MN','Minnesota','Midwest'),
    ('MS','Mississippi','South'), ('MO','Missouri','Midwest'), ('MT','Montana','West'),
    ('NE','Nebraska','Midwest'), ('NV','Nevada','West'), ('NH','New Hampshire','Northeast'),
    ('NJ','New Jersey','Northeast'), ('NM','New Mexico','West'), ('NY','New York','Northeast'),
    ('NC','North Carolina','South'), ('ND','North Dakota','Midwest'), ('OH','Ohio','Midwest'),
    ('OK','Oklahoma','South'), ('OR','Oregon','West'), ('PA','Pennsylvania','Northeast'),
    ('RI','Rhode Island','Northeast'), ('SC','South Carolina','South'), ('SD','South Dakota','Midwest'),
    ('TN','Tennessee','South'), ('TX','Texas','South'), ('UT','Utah','West'),
    ('VT','Vermont','Northeast'), ('VA','Virginia','South'), ('WA','Washington','West'),
    ('WV','West Virginia','South'), ('WI','Wisconsin','Midwest'), ('WY','Wyoming','West'),
    ('PR','Puerto Rico',NULL), ('GU','Guam',NULL), ('VI','Virgin Islands',NULL),
    ('AS','American Samoa',NULL), ('MP','Northern Mariana Islands',NULL)
)
SELECT
    hd.instnm AS School,
    COALESCE(sm.State_name, hd.stabbr, 'Unknown') AS state,
    sm.Region,
    n.*,
    COALESCE(n.baseline_year + 1, NULL) AS Launch_Year,
    CASE 
      WHEN n.diagnostic = 1 AND n.implementation = 0 AND n.accelerator = 0 THEN '1. Diagnostic only'
      WHEN n.diagnostic = 1 AND n.implementation = 1 AND n.accelerator = 0 THEN '2. Diagnostic & Implementation'
      WHEN n.diagnostic = 1 AND n.implementation = 1 AND n.accelerator = 1 THEN '3. Diagnostic, Implementation, & Acceleration Grant'
      WHEN n.diagnostic = 0 AND n.implementation = 1 AND n.accelerator = 1 THEN '3. Diagnostic, Implementation, & Acceleration Grant'
      WHEN n.diagnostic = 1 AND n.implementation = 0 AND n.accelerator = 1 THEN '3. Diagnostic, Implementation, & Acceleration Grant'
      ELSE 'Not Defined'
    END AS Engagement_Type,
    CASE 
      WHEN hd.instsize = 1 THEN 'Very Small (<1,000)'
      WHEN hd.instsize = 2 THEN 'Small (1,000-4,999)'
      WHEN hd.instsize = 3 THEN 'Medium (5,000-9,999)'
      WHEN hd.instsize = 4 THEN 'Large (10,000-19,999)'
      WHEN hd.instsize = 5 THEN 'Very Large (20,000+)'
      ELSE 'Unknown'
    END AS Institution_Size,
    CASE
      WHEN hd.hloffer >= 5 THEN '4-year'
      WHEN hd.hloffer IN (3,4) THEN '2-year'
      WHEN hd.hloffer IN (1,2) THEN '<2-year'
      ELSE NULL
    END AS Institution_Type,
    CASE
      WHEN hd.sector = 0 then 'Administrative Unit'
      WHEN hd.sector = 1 then 'Public, 4-year or above'
      WHEN hd.sector = 2 then 'Private not-for-profit, 4-year or above'
      WHEN hd.sector = 3 then 'Private for-profit, 4-year or above'
      WHEN hd.sector = 4 then 'Public, 2-year'
      WHEN hd.sector = 5 then 'Private not-for-profit, 2-year'
      WHEN hd.sector = 6 then 'Private for-profit, 2-year'
      WHEN hd.sector = 7 then 'Public, less-than 2-year'
      WHEN hd.sector = 8 then 'Private not-for-profit, less-than 2-year'
      WHEN hd.sector = 9 then 'Private for-profit, less-than 2-year'
      WHEN hd.sector = 10 then 'Sector unknown (not active)'
      ELSE 'Unknown'
    END AS Institution_Sector,
    CASE
      WHEN hd.iclevel = 1 THEN 'Four or more years'
      WHEN hd.iclevel = 2 THEN 'At least 2 but less than 4 years'
      WHEN hd.iclevel = 3 THEN 'Less than 2 years (below associate)'
      WHEN hd.iclevel = -3 THEN '{Not available}'
      ELSE 'Unknown'
      END AS Institution_Level,
    CASE
      WHEN hd.control = 1 THEN 'Public'
      WHEN hd.control = 2 THEN 'Private not-for-profit'
      WHEN hd.control = 3 THEN 'Private for-profit'
      WHEN hd.control = -3 THEN '{Not available}'
      ELSE 'Unknown'
      END AS Institution_Control,

    efd_2021.ret_pcf AS ret_2021,
    efd_2022.ret_pcf AS ret_2022,
    efd_2023.ret_pcf AS ret_2023


FROM niss.niss_clients n
LEFT JOIN hd_latest hd
  ON n.unitid = hd.unitid
LEFT JOIN state_map sm
  ON UPPER(hd.stabbr) = sm.stabbr
LEFT JOIN fall_enrollment_efd efd_2021 ON n.unitid = efd_2021.unitid AND efd_2021.year = 2021
LEFT JOIN fall_enrollment_efd efd_2022 ON n.unitid = efd_2022.unitid AND efd_2022.year = 2022
LEFT JOIN fall_enrollment_efd efd_2023 ON n.unitid = efd_2023.unitid AND efd_2023.year = 2023;
