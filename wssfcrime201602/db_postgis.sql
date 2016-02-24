
BEGIN;

ALTER TABLE incident_data ADD COLUMN geom geometry(Point,2227);

UPDATE incident_data SET geom = st_transform(st_setsrid(st_makepoint(lng, lat), 4326), 2227);

CREATE INDEX ON incident_data USING GIST ("geom");


CREATE OR REPLACE VIEW related_incidents AS
WITH inc_cat_data AS (
 SELECT i.id, incident_ts, category, description, res, geom
 FROM incident_data i, category_data c
 WHERE i.label = 'train'
 AND   i.lat < 90
 AND   i.id = c.id ORDER BY RANDOM() LIMIT 50000)
SELECT i1.id keystone_id, i1.incident_ts keystone_ts,
       i2.id, i2.incident_ts, i2.category, i2.description,
       i2.res, st_distance(i1.geom, i2.geom) distance
FROM inc_cat_data i1, inc_cat_data i2
WHERE (i1.incident_ts - i2.incident_ts) <= interval '14 days'
  AND (i2.incident_ts - i1.incident_ts) <= interval '14 days'
  AND st_dwithin(i1.geom, i2.geom, 500)
  AND i1.id <> i2.id;

ALTER VIEW related_incidents OWNER TO geospuser;

COMMIT;
