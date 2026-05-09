-- PostgreSQL script to create and populate public.total_housing_units_by_state
-- Source file: Total Housing Units per State.csv
-- The uploaded CSV is cross-tabbed, so this script stores it in normalized form:
-- one row per state or territory, with Original and Revised values as columns.

CREATE TABLE IF NOT EXISTS public.total_housing_units_by_state (
    state_or_territory TEXT PRIMARY KEY,
    original_housing_units BIGINT NOT NULL,
    revised_housing_units BIGINT NOT NULL,
    source_file TEXT NOT NULL DEFAULT 'Total Housing Units per State.csv',
    loaded_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE public.total_housing_units_by_state IS
    'Total housing units by state or territory, loaded from the uploaded Total Housing Units per State CSV.';

COMMENT ON COLUMN public.total_housing_units_by_state.state_or_territory IS
    'State or territory name from the CSV column headers. Puerto Rico is included, so this is not limited to states only.';

COMMENT ON COLUMN public.total_housing_units_by_state.original_housing_units IS
    'Original total housing units value from the CSV row labeled Original.';

COMMENT ON COLUMN public.total_housing_units_by_state.revised_housing_units IS
    'Revised total housing units value from the CSV row labeled Revised.';

COMMENT ON COLUMN public.total_housing_units_by_state.source_file IS
    'Name of the source CSV file used to load the table.';

COMMENT ON COLUMN public.total_housing_units_by_state.loaded_at IS
    'Timestamp when the row was inserted or last updated by this script.';

INSERT INTO public.total_housing_units_by_state (
    state_or_territory,
    original_housing_units,
    revised_housing_units
)
VALUES
    ('Alabama', 1963711, 1963834),
    ('Alaska', 260978, 260963),
    ('California', 12214549, 12214550),
    ('Colorado', 1808037, 1808358),
    ('Connecticut', 1385975, 1385997),
    ('Florida', 7302947, 7303108),
    ('Georgia', 3281737, 3281866),
    ('Idaho', 527824, 527825),
    ('Illinois', 4885615, 4885744),
    ('Indiana', 2532319, 2532327),
    ('Iowa', 1232511, 1232530),
    ('Kansas', 1131200, 1131395),
    ('Kentucky', 1750927, 1751118),
    ('Louisiana', 1847181, 1847174),
    ('Maryland', 2145283, 2145290),
    ('Massachusetts', 2621989, 2621993),
    ('Michigan', 4234279, 4234252),
    ('Minnesota', 2065946, 2065952),
    ('Mississippi', 1161953, 1161952),
    ('Missouri', 2442017, 2442003),
    ('Nebraska', 722668, 722669),
    ('New Jersey', 3310275, 3310274),
    ('New York', 7679307, 7679307),
    ('North Carolina', 3523944, 3522330),
    ('North Dakota', 289677, 289678),
    ('Ohio', 4783051, 4783066),
    ('Oklahoma', 1514400, 1514399),
    ('Oregon', 1452709, 1452724),
    ('Pennsylvania', 5249750, 5249751),
    ('South Carolina', 1753670, 1753586),
    ('Tennessee', 2439443, 2439435),
    ('Texas', 8157575, 8157557),
    ('Utah', 768594, 768603),
    ('Virginia', 2904192, 2904432),
    ('Washington', 2451075, 2451081),
    ('West Virginia', 844623, 844626),
    ('Wisconsin', 2321144, 2321157),
    ('Puerto Rico', 1418476, 1418474)
ON CONFLICT (state_or_territory) DO UPDATE SET
    original_housing_units = EXCLUDED.original_housing_units,
    revised_housing_units = EXCLUDED.revised_housing_units,
    source_file = 'Total Housing Units per State.csv',
    loaded_at = CURRENT_TIMESTAMP;
