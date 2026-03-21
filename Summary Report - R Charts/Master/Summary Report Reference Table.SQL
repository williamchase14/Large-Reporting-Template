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
    CASE
      WHEN hd.hloffer >= 5 THEN '4-year'
      WHEN hd.hloffer IN (3,4) THEN '2-year'
      WHEN hd.hloffer IN (1,2) THEN '<2-year'
      ELSE NULL
    END AS Institution_Type,
    n.*,
    CASE 
      WHEN n.diagnostic = 1 AND n.implementation = 0 AND n.accelerator = 0 THEN '1. Diagnostic only'
      WHEN n.diagnostic = 1 AND n.implementation = 1 AND n.accelerator = 0 THEN '2. Diagnostic & Implementation'
      WHEN n.diagnostic = 1 AND n.implementation = 1 AND n.accelerator = 1 THEN '3. Diagnostic, Implementation, & Acceleration Grant'
      WHEN n.diagnostic = 0 AND n.implementation = 1 AND n.accelerator = 1 THEN '3. Diagnostic, Implementation, & Acceleration Grant'
      WHEN n.diagnostic = 1 AND n.implementation = 0 AND n.accelerator = 1 THEN '3. Diagnostic, Implementation, & Acceleration Grant'
      ELSE 'Not Defined'
    END AS Engagement_Type,
    
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
