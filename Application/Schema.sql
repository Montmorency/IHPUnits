CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE TABLE unit_conversions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    source_unit_expr TEXT DEFAULT '' NOT NULL,
    target_unit_expression TEXT NOT NULL,
    source_number DOUBLE PRECISION NOT NULL,
    target_number DOUBLE PRECISION NOT NULL
);
