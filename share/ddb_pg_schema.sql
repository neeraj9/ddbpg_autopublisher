
/* run as postgres user */
CREATE EXTENSION IF NOT EXISTS hstore;

CREATE TABLE metrics (
  collection  text NOT NULL,
  metric      text[] NOT NULL,
  bucket      text NOT NULL,
  key         text[] NOT NULL,
  dimensions  hstore
);

CREATE UNIQUE INDEX ON metrics (collection, metric, bucket, key);
CREATE INDEX ON metrics USING btree (collection, akeys(dimensions));
CREATE INDEX ON metrics USING btree (collection, metric, akeys(dimensions));
CREATE INDEX ON metrics USING GIST (dimensions);
