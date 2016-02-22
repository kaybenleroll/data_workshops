
BEGIN;

ALTER TABLE incident_data ADD COLUMN geom geometry(Point,2227);

UPDATE incident_data SET geom = st_transform(st_setsrid(st_makepoint(lng, lat), 4326), 2227);

CREATE INDEX ON incident_data USING GIST ("geom");


CREATE VIEW related_incidents AS
WITH inc_cat_data AS (
 SELECT i.id, incident_ts, category, description, res, geom
 FROM incident_data i, category_data c
 WHERE i.label = 'train'
 AND   i.id = c.id)
SELECT i1.id orig_id, i1.incident_ts orig_ts, i2.*
FROM inc_cat_data i1, inc_cat_data i2
WHERE (i1.incident_ts - i2.incident_ts) < interval '14 days'
  AND st_dwithin(i1.geom, i2.geom, 300);

ALTER VIEW related_incidents OWNER TO geospuser;

COMMIT;
