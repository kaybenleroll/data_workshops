CREATE EXTENSION postgis;

BEGIN;

CREATE TABLE category_data (
    id text,
    category text,
    description text,
    res text
);


ALTER TABLE category_data OWNER TO geospuser;

ALTER TABLE ONLY category_data
    ADD CONSTRAINT category_data_pkey PRIMARY KEY (id);


--
-- Name: incident_data; Type: TABLE; Schema: public; Owner: geospuser; Tablespace:
--

CREATE TABLE incident_data (
    id text,
    label text,
    incident_ts timestamp with time zone,
    dow text,
    district text,
    address text,
    lng double precision,
    lat double precision
);

ALTER TABLE incident_data OWNER TO geospuser;

ALTER TABLE ONLY incident_data
    ADD CONSTRAINT incident_data_pkey PRIMARY KEY (id);

COMMIT;
