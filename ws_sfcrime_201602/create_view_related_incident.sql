WITH inc_cat_data AS (
 SELECT i.id, incident_ts, category, description, res
 FROM incident_data i, category_data c
 WHERE i.label = 'train'
 AND   i.id = c.id)
SELECT i1.id orig_id, i1.incident_ts orig_ts, i2.*
FROM inc_cat_data i1, inc_cat_data i2
WHERE i1.incident_ts - i2.incident_ts < interval '14 days'
  AND i1.id = 'T00000009' LIMIT 10;
