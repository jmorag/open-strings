-- Dump of composer, work, and movement tables 08-27-2020 for seeding remote db
--
-- PostgreSQL database dump
--

-- Dumped from database version 11.7
-- Dumped by pg_dump version 11.7

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Data for Name: composers; Type: TABLE DATA; Schema: public; Owner: joseph
--

COPY public.composers (id, name, url) FROM stdin;
7	Beethoven,_Ludwig_van	https://imslp.org/wiki/Category:Beethoven,_Ludwig_van
8	Brahms,_Johannes	https://imslp.org/wiki/Category:Brahms,_Johannes
9	Prokofiev,_Sergey	https://imslp.org/wiki/Category:Prokofiev,_Sergey
10	Sibelius,_Jean	https://imslp.org/wiki/Category:Sibelius,_Jean
11	Ysaÿe,_Eugène	https://imslp.org/wiki/Category:Ysaÿe,_Eugène
14	Mozart,_Wolfgang_Amadeus	https://imslp.org/wiki/Category:Mozart,_Wolfgang_Amadeus
15	Strauss,_Richard	https://imslp.org/wiki/Category:Strauss,_Richard
16	Cimador,_Giambattista	https://imslp.org/wiki/Category:Cimador,_Giambattista
17	Bottesini,_Giovanni	https://imslp.org/wiki/Category:Bottesini,_Giovanni
18	Koussevitzky,_Serge	https://imslp.org/wiki/Category:Koussevitzky,_Serge
19	Tchaikovsky,_Pyotr	https://imslp.org/wiki/Category:Tchaikovsky,_Pyotr
20	Ravel,_Maurice	https://imslp.org/wiki/Category:Ravel,_Maurice
22	Dvořák,_Antonín	https://imslp.org/wiki/Category:Dvořák,_Antonín
23	Schumann,_Robert	https://imslp.org/wiki/Category:Schumann,_Robert
24	Schoenberg,_Arnold	https://imslp.org/wiki/Category:Schoenberg,_Arnold
12	Bartók,_Béla	https://imslp.org/wiki/Category:Bartók,_Béla
13	Bach,_Johann_Sebastian	https://imslp.org/wiki/Category:Bach,_Johann_Sebastian
\.


--
-- Data for Name: works; Type: TABLE DATA; Schema: public; Owner: joseph
--

COPY public.works (id, title, url, instrumentation, composer_id) FROM stdin;
135	Violin_Concerto_No.2,_Sz.112	https://imslp.org/wiki/Violin_Concerto_No.2%2C_Sz.112_(Bartók%2C_Béla)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	12
137	Violin_Sonata_No.1_in_G_minor,_BWV_1001	https://imslp.org/wiki/Violin_Sonata_No.1_in_G_minor,_BWV_1001_(Bach,_Johann_Sebastian)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	13
140	Violin_Partita_No.2_in_D_minor,_BWV_1004	https://imslp.org/wiki/Violin_Partita_No.2_in_D_minor,_BWV_1004_(Bach,_Johann_Sebastian)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	13
142	Violin_Partita_No.3_in_E_major,_BWV_1006	https://imslp.org/wiki/Violin_Partita_No.3_in_E_major,_BWV_1006_(Bach,_Johann_Sebastian)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	13
144	Cello_Suite_No.2_in_D_minor,_BWV_1008	https://imslp.org/wiki/Cello_Suite_No.2_in_D_minor,_BWV_1008_(Bach,_Johann_Sebastian)	["sPart {instrument = Cello, solo = Solo, part_num = 0}"]	13
146	Cello_Suite_No.4_in_E-flat_major,_BWV_1010	https://imslp.org/wiki/Cello_Suite_No.4_in_E-flat_major,_BWV_1010_(Bach,_Johann_Sebastian)	["sPart {instrument = Cello, solo = Solo, part_num = 0}"]	13
148	Cello_Suite_No.6_in_D_major,_BWV_1012	https://imslp.org/wiki/Cello_Suite_No.6_in_D_major,_BWV_1012_(Bach,_Johann_Sebastian)	["sPart {instrument = Cello, solo = Solo, part_num = 0}"]	13
150	Violin_Concerto_No.1_in_B-flat_major,_K.207	https://imslp.org/wiki/Violin_Concerto_No.1_in_B-flat_major,_K.207_(Mozart,_Wolfgang_Amadeus)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	14
152	Violin_Concerto_No.3_in_G_major,_K.216	https://imslp.org/wiki/Violin_Concerto_No.3_in_G_major,_K.216_(Mozart,_Wolfgang_Amadeus)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	14
154	Violin_Concerto_No.5_in_A_major,_K.219	https://imslp.org/wiki/Violin_Concerto_No.5_in_A_major,_K.219_(Mozart,_Wolfgang_Amadeus)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	14
156	Don_Juan,_Op.20	https://imslp.org/wiki/Don_Juan,_Op.20_(Strauss,_Richard)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	15
158	Double_Bass_Concerto	https://imslp.org/wiki/Double_Bass_Concerto_(Cimador%2C_Giambattista)	["sPart {instrument = Bass, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	16
160	Double_Bass_Concerto_No.2_in_B_minor	https://imslp.org/wiki/Double_Bass_Concerto_No.2_in_B_minor_(Bottesini%2C_Giovanni)	["sPart {instrument = Bass, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	17
162	Double_Bass_Concerto,_Op.3	https://imslp.org/wiki/Double_Bass_Concerto%2C_Op.3_(Koussevitzky%2C_Serge)	["sPart {instrument = Bass, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	18
164	Violin_Concerto,_Op.35	https://imslp.org/wiki/Violin_Concerto,_Op.35_(Tchaikovsky,_Pyotr)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	19
166	Metamorphosen,_TrV_290	https://imslp.org/wiki/Metamorphosen%2C_TrV_290_(Strauss%2C_Richard)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Violin, solo = Solo, part_num = 3}","sPart {instrument = Violin, solo = Solo, part_num = 4}","sPart {instrument = Violin, solo = Solo, part_num = 5}","sPart {instrument = Violin, solo = Solo, part_num = 6}","sPart {instrument = Violin, solo = Solo, part_num = 7}","sPart {instrument = Violin, solo = Solo, part_num = 8}","sPart {instrument = Violin, solo = Solo, part_num = 9}","sPart {instrument = Violin, solo = Solo, part_num = 10}","sPart {instrument = Viola, solo = Solo, part_num = 1}","sPart {instrument = Viola, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 3}","sPart {instrument = Viola, solo = Solo, part_num = 4}","sPart {instrument = Viola, solo = Solo, part_num = 5}","sPart {instrument = Cello, solo = Solo, part_num = 1}","sPart {instrument = Cello, solo = Solo, part_num = 2}","sPart {instrument = Cello, solo = Solo, part_num = 3}","sPart {instrument = Cello, solo = Solo, part_num = 4}","sPart {instrument = Cello, solo = Solo, part_num = 5}","sPart {instrument = Bass, solo = Solo, part_num = 1}","sPart {instrument = Bass, solo = Solo, part_num = 2}","sPart {instrument = Bass, solo = Solo, part_num = 3}"]	15
170	Violin_Concerto_in_D_minor,_WoO_23	https://imslp.org/wiki/Violin_Concerto_in_D_minor,_WoO_23_(Schumann,_Robert)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	23
136	Violin_Concerto_No.1,_Sz.36	https://imslp.org/wiki/Violin_Concerto_No.1,_Sz.36_(Bartók,_Béla)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	12
138	Violin_Partita_No.1_in_B_minor,_BWV_1002	https://imslp.org/wiki/Violin_Partita_No.1_in_B_minor,_BWV_1002_(Bach,_Johann_Sebastian)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	13
139	Violin_Sonata_No.2_in_A_minor,_BWV_1003	https://imslp.org/wiki/Violin_Sonata_No.2_in_A_minor,_BWV_1003_(Bach,_Johann_Sebastian)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	13
141	Violin_Sonata_No.3_in_C_major,_BWV_1005	https://imslp.org/wiki/Violin_Sonata_No.3_in_C_major,_BWV_1005_(Bach,_Johann_Sebastian)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	13
143	Cello_Suite_No.1_in_G_major,_BWV_1007	https://imslp.org/wiki/Cello_Suite_No.1_in_G_major,_BWV_1007_(Bach,_Johann_Sebastian)	["sPart {instrument = Cello, solo = Solo, part_num = 0}"]	13
145	Cello_Suite_No.3_in_C_major,_BWV_1009	https://imslp.org/wiki/Cello_Suite_No.3_in_C_major,_BWV_1009_(Bach,_Johann_Sebastian)	["sPart {instrument = Cello, solo = Solo, part_num = 0}"]	13
147	Cello_Suite_No.5_in_C_minor,_BWV_1011	https://imslp.org/wiki/Cello_Suite_No.5_in_C_minor,_BWV_1011_(Bach,_Johann_Sebastian)	["sPart {instrument = Cello, solo = Solo, part_num = 0}"]	13
149	Concerto_for_2_Violins_in_D_minor,_BWV_1043	https://imslp.org/wiki/Concerto_for_2_Violins_in_D_minor%2C_BWV_1043_(Bach%2C_Johann_Sebastian)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	13
151	Violin_Concerto_No.2_in_D_major,_K.211	https://imslp.org/wiki/Violin_Concerto_No.2_in_D_major,_K.211_(Mozart,_Wolfgang_Amadeus)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	14
153	Violin_Concerto_No.4_in_D_major,_K.218	https://imslp.org/wiki/Violin_Concerto_No.4_in_D_major,_K.218_(Mozart,_Wolfgang_Amadeus)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	14
155	Sinfonia_concertante_in_E-flat_major,_K.364/320d	https://imslp.org/wiki/Sinfonia_concertante_in_E-flat_major,_K.364/320d_(Mozart,_Wolfgang_Amadeus)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	14
157	Also_sprach_Zarathustra,_Op.30	https://imslp.org/wiki/Also_sprach_Zarathustra,_Op.30_(Strauss,_Richard)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	15
159	Double_Bass_Concerto_No.1_in_F-sharp_minor	https://imslp.org/wiki/Double_Bass_Concerto_No.1_in_F-sharp_minor_(Bottesini%2C_Giovanni)	["sPart {instrument = Bass, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	17
161	Gran_Duo_Concertante	https://imslp.org/wiki/Gran_Duo_Concertante_(Bottesini%2C_Giovanni)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Bass, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	17
163	Sonata_for_Violin_Solo,_Sz.117	https://imslp.org/wiki/Sonata_for_Violin_Solo%2C_Sz.117_(Bartók%2C_Béla)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	12
165	Tzigane	https://imslp.org/wiki/Tzigane_(Ravel%2C_Maurice)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	20
167	Ein_Heldenleben,_Op.40	https://imslp.org/wiki/Ein_Heldenleben,_Op.40_(Strauss,_Richard)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	15
169	Violin_Concerto,_Op.53	https://imslp.org/wiki/Violin_Concerto%2C_Op.53_(Dvořák%2C_Antonín)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}"]	22
171	Verklärte_Nacht,_Op.4	https://imslp.org/wiki/Verklärte_Nacht,_Op.4_(Schoenberg,_Arnold)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 1}","sPart {instrument = Viola, solo = Solo, part_num = 2}","sPart {instrument = Cello, solo = Solo, part_num = 1}","sPart {instrument = Cello, solo = Solo, part_num = 2}"]	24
68	Violin_Sonata_No.1,_Op.12_No.1	https://imslp.org/wiki/Violin_Sonata_No.1,_Op.12_No.1_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
69	Violin_Sonata_No.2,_Op.12_No.2	https://imslp.org/wiki/Violin_Sonata_No.2,_Op.12_No.2_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
70	Violin_Sonata_No.3,_Op.12_No.3	https://imslp.org/wiki/Violin_Sonata_No.3,_Op.12_No.3_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
71	Violin_Sonata_No.4,_Op.23	https://imslp.org/wiki/Violin_Sonata_No.4,_Op.23_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
72	Violin_Sonata_No.5,_Op.24	https://imslp.org/wiki/Violin_Sonata_No.5,_Op.24_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
73	Violin_Sonata_No.6,_Op.30_No.1	https://imslp.org/wiki/Violin_Sonata_No.6,_Op.30_No.1_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
74	Violin_Sonata_No.7,_Op.30_No.2	https://imslp.org/wiki/Violin_Sonata_No.7,_Op.30_No.2_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
75	Violin_Sonata_No.8,_Op.30_No.3	https://imslp.org/wiki/Violin_Sonata_No.8,_Op.30_No.3_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
76	Violin_Sonata_No.9,_Op.47	https://imslp.org/wiki/Violin_Sonata_No.9,_Op.47_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
77	Violin_Sonata_No.10,_Op.96	https://imslp.org/wiki/Violin_Sonata_No.10,_Op.96_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	7
78	Violin_Concerto_in_D_major,_Op.61	https://imslp.org/wiki/Violin_Concerto_in_D_major,_Op.61_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
79	Romance_in_G_major,_Op.40	https://imslp.org/wiki/Romance_in_G_major,_Op.40_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
80	Romance_in_F_major,_Op.50	https://imslp.org/wiki/Romance_in_F_major,_Op.50_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
81	Triple_Concerto,_Op.56	https://imslp.org/wiki/Triple_Concerto,_Op.56_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
82	Piano_Concerto_No.1,_Op.15	https://imslp.org/wiki/Piano_Concerto_No.1,_Op.15_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
83	Piano_Concerto_No.2,_Op.19	https://imslp.org/wiki/Piano_Concerto_No.2,_Op.19_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
84	Piano_Concerto_No.3,_Op.37	https://imslp.org/wiki/Piano_Concerto_No.3,_Op.37_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
85	Piano_Concerto_No.4,_Op.58	https://imslp.org/wiki/Piano_Concerto_No.4,_Op.58_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
86	Piano_Concerto_No.5,_Op.73	https://imslp.org/wiki/Piano_Concerto_No.5,_Op.73_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
87	Symphony_No.1,_Op.21	https://imslp.org/wiki/Symphony_No.1,_Op.21_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
88	Symphony_No.2,_Op.36	https://imslp.org/wiki/Symphony_No.2,_Op.36_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
89	Symphony_No.3,_Op.55	https://imslp.org/wiki/Symphony_No.3,_Op.55_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
90	Symphony_No.4,_Op.60	https://imslp.org/wiki/Symphony_No.4,_Op.60_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
91	Symphony_No.5,_Op.67	https://imslp.org/wiki/Symphony_No.5,_Op.67_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
92	Symphony_No.6,_Op.68	https://imslp.org/wiki/Symphony_No.6,_Op.68_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
93	Symphony_No.7,_Op.92	https://imslp.org/wiki/Symphony_No.7,_Op.92_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
94	Symphony_No.8,_Op.93	https://imslp.org/wiki/Symphony_No.8,_Op.93_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
95	Symphony_No.9,_Op.125	https://imslp.org/wiki/Symphony_No.9,_Op.125_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	7
96	String_Quartet_No.1,_Op.18_No.1	https://imslp.org/wiki/String_Quartet_No.1,_Op.18_No.1_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
97	String_Quartet_No.2,_Op.18_No.2	https://imslp.org/wiki/String_Quartet_No.2,_Op.18_No.2_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
98	String_Quartet_No.3,_Op.18_No.3	https://imslp.org/wiki/String_Quartet_No.3,_Op.18_No.3_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
99	String_Quartet_No.4,_Op.18_No.4	https://imslp.org/wiki/String_Quartet_No.4,_Op.18_No.4_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
100	String_Quartet_No.5,_Op.18_No.5	https://imslp.org/wiki/String_Quartet_No.5,_Op.18_No.5_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
101	String_Quartet_No.6,_Op.18_No.6	https://imslp.org/wiki/String_Quartet_No.6,_Op.18_No.6_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
102	String_Quartet_No.7,_Op.59_No.1	https://imslp.org/wiki/String_Quartet_No.7,_Op.59_No.1_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
103	String_Quartet_No.8,_Op.59_No.2	https://imslp.org/wiki/String_Quartet_No.8,_Op.59_No.2_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
104	String_Quartet_No.9,_Op.59_No.3	https://imslp.org/wiki/String_Quartet_No.9,_Op.59_No.3_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
105	String_Quartet_No.10,_Op.74	https://imslp.org/wiki/String_Quartet_No.10,_Op.74_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
106	String_Quartet_No.11,_Op.95	https://imslp.org/wiki/String_Quartet_No.11,_Op.95_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
107	String_Quartet_No.12,_Op.127	https://imslp.org/wiki/String_Quartet_No.12,_Op.127_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
108	String_Quartet_No.13,_Op.130	https://imslp.org/wiki/String_Quartet_No.13,_Op.130_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
109	Große_Fuge,_Op.133	https://imslp.org/wiki/Große_Fuge,_Op.133_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
110	String_Quartet_No.14,_Op.131	https://imslp.org/wiki/String_Quartet_No.14,_Op.131_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
134	Solo_Sonata_No._6,_Op._27_(A_Manuel_Quiroga)	https://imslp.org/wiki/6_Sonatas_for_Solo_Violin,_Op.27_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	11
111	String_Quartet_No.15,_Op.132	https://imslp.org/wiki/String_Quartet_No.15,_Op.132_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
112	String_Quartet_No.16,_Op.135	https://imslp.org/wiki/String_Quartet_No.16,_Op.135_(Beethoven,_Ludwig_van)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Viola, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}"]	7
113	Violin_Concerto,_Op.77	https://imslp.org/wiki/Violin_Concerto,_Op.77_(Brahms,_Johannes)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	8
114	Concerto_for_Violin_and_Cello,_Op.102	https://imslp.org/wiki/Concerto_for_Violin_and_Cello,_Op.102_(Brahms,_Johannes)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	8
115	Violin_Concerto_No.1,_Op.19	https://imslp.org/wiki/Violin_Concerto_No.1,_Op.19_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	9
116	Violin_Concerto_No.2,_Op.63	https://imslp.org/wiki/Violin_Concerto_No.2,_Op.63_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	9
117	Sonata_for_2_Violins,_Op.56	https://imslp.org/wiki/Sonata_for_2_Violins,_Op.56_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}"]	9
118	Violin_Sonata_No.1,_Op.80	https://en.wikipedia.org/wiki/Violin_Sonata_No._1_(Prokofiev)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	9
119	Violin_Sonata_No.2,_Op.94bis	https://imslp.org/wiki/Violin_Sonata_No.2,_Op.94bis_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	9
120	Sonata_for_Solo_Violin,_Op.115	https://imslp.org/wiki/Sonata_for_Solo_Violin,_Op.115_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	9
121	Cello_Sonata,_Op.119	https://imslp.org/wiki/Cello_Sonata,_Op.119_(Prokofiev,_Sergey)	["sPart {instrument = Cello, solo = Solo, part_num = 0}"]	9
122	Sinfonia_concertante,_Op.125	https://imslp.org/wiki/Sinfonia_concertante,_Op.125_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Solo, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	9
123	Symphony_No.1,_Op.25	https://imslp.org/wiki/Symphony_No.1,_Op.25_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	9
124	Symphony_No.5,_Op.100	https://imslp.org/wiki/Symphony_No.5,_Op.100_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	9
125	Symphony_No.7,_Op.131	https://imslp.org/wiki/Symphony_No.7,_Op.131_(Prokofiev,_Sergey)	["sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	9
126	Violin_Concerto,_Op.47	https://imslp.org/wiki/Violin_Concerto,_Op.47_(Sibelius,_Jean)	["sPart {instrument = Violin, solo = Solo, part_num = 0}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	10
127	Amitié,_Op.26	https://imslp.org/wiki/Amitié,_Op.26_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 1}","sPart {instrument = Violin, solo = Solo, part_num = 2}","sPart {instrument = Violin, solo = Tutti, part_num = 1}","sPart {instrument = Violin, solo = Tutti, part_num = 2}","sPart {instrument = Viola, solo = Tutti, part_num = 0}","sPart {instrument = Cello, solo = Tutti, part_num = 0}","sPart {instrument = Bass, solo = Tutti, part_num = 0}"]	11
128	Poème_élégiaque,_Op.12	https://imslp.org/wiki/Poème_élégiaque,_Op.12_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	11
129	Solo Sonata No. 1, Op. 27 (A Joseph Szigeti)	https://imslp.org/wiki/6_Sonatas_for_Solo_Violin,_Op.27_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	11
130	Solo_Sonata_No._2,_Op._27_(A_Jacques_Thibaud)	https://imslp.org/wiki/6_Sonatas_for_Solo_Violin,_Op.27_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	11
131	Solo_Sonata_No._3,_Op._27_(A_Georges_Enesco)	https://imslp.org/wiki/6_Sonatas_for_Solo_Violin,_Op.27_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	11
132	Solo_Sonata_No._4,_Op._27_(A_Fritz_Kreisler)	https://imslp.org/wiki/6_Sonatas_for_Solo_Violin,_Op.27_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	11
133	Solo_Sonata_No._5,_Op._27_(A_Mathieu_Crickboom)	https://imslp.org/wiki/6_Sonatas_for_Solo_Violin,_Op.27_(Ysaÿe,_Eugène)	["sPart {instrument = Violin, solo = Solo, part_num = 0}"]	11
\.


--
-- Data for Name: movements; Type: TABLE DATA; Schema: public; Owner: joseph
--

COPY public.movements (id, number, name, work_id) FROM stdin;
630	2	Scherzo. Vivacissimo	115
697	1	Allemanda	138
698	2	Double	138
699	3	Courante	138
700	4	Double	138
701	5	Sarabande	138
702	6	Double	138
703	7	Tempo di Bourrée	138
704	8	Double	138
705	1	Grave	139
706	2	Fuga	139
707	3	Andante	139
708	4	Allegro	139
725	1	Prélude	143
726	2	Allemande	143
727	3	Courante	143
728	4	Sarabande	143
729	5	Menuett I	143
730	6	Menuett II	143
731	7	Gigue	143
753	1	Prélude	147
754	2	Allemande	147
755	3	Courante	147
756	4	Sarabande	147
757	5	Gavotte I	147
758	6	Gavotte II	147
759	7	Gigue	147
773	1	Allegro moderato	151
774	2	Andante	151
775	3	Allegro	151
785	1	Allegro maestoso	155
786	2	Andante	155
787	3	Presto	155
793	1	Allegro moderato	159
794	2	Andantino	159
795	3	Finale. Allegro con fuoco	159
803	1	Tempo di ciaccona	163
804	2	Fuga. Risoluto, non troppo vivo	163
805	3	Melodia. Adagio	163
806	4	Presto	163
812	0		167
817	1	In kräftigem, nicht zu schnellem Tempo	170
818	2	Langsam	170
819	3	Lebhaft, doch nicht schnell	170
688	1	Allegro non troppo	135
689	2	Andante tranquillo	135
690	3	Allegro molto	135
709	1	Allemande	140
710	2	Courante	140
711	3	Sarabande	140
712	4	Gigue	140
713	5	Chaconne	140
732	1	Prélude	144
733	2	Allemande	144
734	3	Courante	144
735	4	Sarabande	144
736	5	Menuett I	144
737	6	Menuett II	144
738	7	Gigue	144
760	1	Prélude	148
761	2	Allemande	148
762	3	Courante	148
763	4	Sarabande	148
764	5	Gavotte I	148
765	6	Gavotte II	148
766	7	Gigue	148
776	1	Allegro	152
777	2	Adagio	152
778	3	Rondeau. Allegro	152
788	0		156
796	1	Allegro	160
797	2	Andante	160
798	3	Allegro	160
807	1	Allegro moderato - Moderato assai	164
808	2	Andante	164
809	3	Allegro vivacissimo	164
814	1	Allegro ma non troppo	169
815	2	Adagio ma non troppo	169
816	3	Allegro giocoso, ma non troppo	169
820	0		171
691	1	Andante sostenuto	136
692	2	Allegro giocoso	136
714	1	Adagio	141
715	2	Fuga	141
716	3	Largo	141
717	4	Allegro assai	141
739	1	Prélude	145
740	2	Allemande	145
741	3	Courante	145
742	4	Sarabande	145
743	5	Bourrée I	145
744	6	Bourrée II	145
745	7	Gigue	145
767	1	Vivace	149
768	2	Largo, ma non tanto	149
769	3	Allegro	149
779	1	Allegro	153
780	2	Andante cantabile	153
781	3	Rondeau: Andante grazioso	153
789	0		157
799	0		161
810	0		165
561	2	Adagio con moto	98
562	3	Allegro	98
563	4	Presto	98
564	1	Allegro, ma non tanto	99
565	2	Scherzo. Andante scherzoso quasi Allegretto	99
566	3	Minuet. Allegretto	99
567	4	Allegro	99
568	1	Allegro	100
569	2	Minuet - Trio	100
570	3	Andante cantabile con variazioni	100
571	4	Allegro	100
572	1	Allegro con brio	101
573	2	Adagio, ma non troppo	101
574	3	La Malinconia. Adagio	101
575	4	Allegretto quasi Allegro	101
576	1	Allegro	102
577	2	Allegretto vivace e sempre scherzando	102
578	3	Adagio molto e mesto	102
579	4	Thème russe. Allegro	102
580	1	Allegro	103
581	2	Molto Adagio. Si tratta questo pezzo con molto di sentimento	103
582	3	Allegretto - Maggiore, Thème russe.	103
583	4	Finale. Presto	103
584	1	Introduzione. Andante con moto - Allegro vivace	104
585	2	Andante con moto quasi Allegretto	104
586	3	Minuet. Grazioso - Trio	104
587	4	Allegro molto	104
588	1	Poco adagio - Allegro	105
589	2	Adagio ma non troppo	105
590	3	Presto - Più presto quasi prestissimo	105
591	4	Allegretto con variazioni	105
592	1	Allegro con brio	106
593	2	Allegretto ma non troppo	106
594	3	Allegro assai vivace ma serioso - Trio	106
595	4	Larghetto espressivo - Allegretto agitato	106
596	1	Maestoso - Allegro	107
597	2	Adagio, ma non troppo e molto cantabile	107
598	3	Scherzando vivace - Trio	107
599	4	Finale. Allegro	107
600	1	Adagio ma non troppo - Allegro	108
601	2	Presto	108
602	3	Andante con moto ma non troppo	108
603	4	Alla danza tedesca. Allegro assai	108
604	5	Cavatina. Adagio molto espressivo	108
605	6	Finale. Allegro	108
606	0	Allegro	109
607	1	Adagio ma non troppo e molto espressivo	110
608	2	Allegro molto vivace	110
609	3	Allegro moderato (recitative)	110
610	4	Andante ma non troppo e molto cantabile	110
611	5	Presto	110
612	6	Adagio quasi un poco andante	110
613	7	Allegro	110
614	1	Assai sostenuto - Allegro	111
615	2	Allegro ma non tanto	111
616	3	Molto adagio	111
617	4	Alla marcia, assai vivace	111
618	5	Allegro appassionato	111
619	1	Allegretto	112
620	2	Vivace	112
621	3	Lento assai, cantante e tranquillo	112
622	4	Grave, ma non troppo tratto - Allegro	112
623	1	Allegro non troppo	113
624	2	Adagio	113
625	3	Allegro giocoso, ma non troppo	113
626	1	Allegro	114
627	2	Andante	114
628	3	Vivace non troppo	114
629	1	Andantino	115
631	3	Moderato. Allegro moderato	115
632	1	Allegro moderato	116
633	2	Andante assai	116
634	3	Allegro ben marcato	116
635	1	Andante cantabile	117
636	2	Allegro	117
637	3	Commodo (quasi allegretto)	117
638	4	Allegro con brio	117
639	1	Andante assai	118
640	2	Allegro brusco	118
641	3	Andante	118
642	4	Allegrissimo - Andate assai, come prima	118
643	1	Moderato	119
644	2	Presto - Poco piu mosso del - Tempo I	119
645	3	Andante	119
646	4	Allegro con brio - Poco meno mosso - Tempo I - Poco meno mosso - Allegro con brio	119
647	1	Moderato	120
648	2	Andante dolce. Tema con variazioni	120
649	3	Con brio. Allegro precipitato	120
650	1	Andante grave - Moderato animato - Andante - Andante grave, come prima	121
651	2	Moderato - Andante dolce - Moderato primo	121
652	3	Allegro, ma non troppo	121
653	1	Andante	122
654	2	Allegro giusto	122
655	3	Andante con moto	122
656	1	Allegro	123
657	2	Larghetto	123
658	3	Gavotte. Non troppo allegro	123
659	4	Finale. Molto vivace	123
660	1	Andante	124
459	1	Allegro con brio	68
460	2	Tema con variazioni. Andante con moto	68
461	3	Rondo - Allegro	68
462	1	Allegro vivace	69
463	2	Andante, più tosto Allegretto	69
464	3	Allegro piacevole	69
465	1	Allegro con spirito	70
466	2	Adagio con molt' espressione	70
467	3	Rondo. Allegro molto	70
468	1	Presto	71
469	2	Andante scherzoso, più Allegretto	71
470	3	Allegro molto	71
471	1	Allegro	72
472	2	Adagio molto espressivo	72
473	3	Scherzo. Allegro molto - Trio	72
474	4	Rondo. Allegro ma non troppo	72
475	1	Allegro	73
476	2	Adagio molto espressivo	73
477	3	Allegretto con variazioni	73
478	1	Allegro con brio	74
479	2	Adagio cantabile	74
480	3	Scherzo. Allegro - Trio	74
481	4	Finale. Allegro	74
482	1	Allegro assai	75
483	2	Tempo di Minuetto, ma molto moderato e grazioso	75
484	3	Allegro vivace	75
485	1	Adagio sostenuto - Presto	76
486	2	Andante con variazioni	76
487	3	Finale. Presto	76
488	1	Allegro moderato	77
489	2	Adagio espressivo	77
490	3	Scherzo. Allegro - Trio	77
491	4	Poco Allegretto	77
492	1	Allegro ma non troppo	78
493	2	Larghetto	78
494	3	Rondo. Allegro	78
495	0		79
496	0		80
497	1	Allegro	81
498	2	Largo	81
499	3	Rondo alla Polacca	81
500	1	Allegro con brio	82
501	2	Largo	82
502	3	Rondo. Allegro	82
503	1	Allegro con brio	83
504	2	Adagio	83
505	3	Rondo. Molto Allegro	83
506	1	Allegro con brio	84
507	2	Largo	84
508	3	Rondo. Allegro - Presto	84
509	1	Allegro moderato	85
510	2	Andante con moto	85
511	3	Rondo. Vivace	85
512	1	Allegro	86
513	2	Andante un pocco moto	86
514	3	Rondo. Allegro	86
515	1	Adagio molto - Allegro con brio	87
516	2	Andante cantabile con moto	87
517	3	Minuet. Allegro molto e vivace - Trio	87
518	4	Finale. Adagio - Allegro molto e vivace	87
519	1	Adagio molto - Allegro con brio	88
520	2	Larghetto	88
521	3	Scherzo. Allegro - Trio	88
522	4	Allegro molto	88
523	1	Allegro con brio	89
524	2	Marcia funebre. Adagio assai	89
525	3	Scherzo. Allegro vivace - Trio	89
526	4	Finale. Allegro molto	89
527	1	Adagio - Allegro vivace	90
528	2	Adagio	90
529	3	Allegro vivace	90
530	4	Allegro ma non troppo	90
531	1	Allegro con brio	91
532	2	Andante con moto	91
533	3	Scherzo. Allegro - Trio	91
534	4	Allegro	91
535	1	Erwachen heiterer Empfindungen bei der Ankunft auf dem Lande. Allegro ma non troppo	92
536	2	Scene am Bach. Andante molto moto	92
537	3	Lustiges Zusammensein der Landleute. Allegro	92
538	4	Gewitter. Sturm. Allegro	92
539	5	Hirtengesang. Frohe und dankbare Gefühle nach dem Sturm. Allegretto	92
540	1	Poco sostenuto - Vivace	93
541	2	Allegretto	93
542	3	Presto	93
543	4	Allegro con brio	93
544	1	Allegro vivace e con brio	94
545	2	Allegretto scherzando	94
546	3	Tempo di Menuetto	94
547	4	Allegro vivace	94
548	1	Allegro ma non troppo, un poco maestoso	95
549	2	Scherzo. Molto vivace - Presto	95
550	3	Adagio molto e cantabile	95
551	4	Presto (D minor) - Allegro assai (D major); Allegro molto assai (Alla marcia) (B♭ major); Andante maestoso (G major) - Adagio ma non troppo, ma divoto (G minor); Allegro energico, sempre ben marcato - Allegro ma non tanto - Pressitissmo (D major)	95
552	1	Allegro con brio	96
553	2	Adagio affettuoso ed appasionato	96
554	3	Scherzo. Allegro molto	96
555	4	Allegro	96
556	1	Allegro	97
557	2	Adagio cantabile	97
558	3	Scherzo. Allegro	97
559	4	Allegro molto, quasi Presto	97
560	1	Allegro	98
661	2	Allegro marcato	124
662	3	Adagio	124
663	4	Allegro giocoso	124
664	1	Moderato	125
665	2	Allegretto	125
666	3	Andante espressivo	125
667	4	Vivace	125
668	1	Allegro moderato	126
669	2	Adagio di molto	126
670	3	Allegro, ma non tanto	126
671	0		127
672	0		128
673	1	Grave. Lento assai	129
674	2	Fugato. Molto moderato	129
675	3	Allegretto poco scherzoso. Amabile	129
676	4	Finale con brio. Allegro fermo	129
677	1	Obsession. Prélude. Poco vivace	130
678	2	Malinconia. Poco lento	130
679	3	Danse des ombres. Sarabande (lento)	130
680	4	Les furies. Allegro furioso	130
681	0	Ballade. Lento molto sostenuto in modo di recitativo - Molto moderato quasi lento - Allegro in tempo giusto e con bravura	131
682	1	Allemanda. Lento maestoso	132
683	2	Sarabande. Quasi lento	132
684	3	Finale. Presto ma non troppo	132
685	1	L'Aurore. Lento assai	133
686	2	Danse rustique. Allegro giocoso molto moderato	133
687	0	Allegro giusto non troppo vivo - Allegretto poco scherzando - Allegro Tempo I	134
693	1	Adagio	137
694	2	Fuga	137
695	3	Siciliana	137
696	4	Presto	137
718	1	Preludio	142
719	2	Loure	142
720	3	Gavotte en Rondeau	142
721	4	Menuett I	142
722	5	Menuett II	142
723	6	Bourrée	142
724	7	Gigue	142
746	1	Prélude	146
747	2	Allemande	146
748	3	Courante	146
749	4	Sarabande	146
750	5	Bourrée I	146
751	6	Bourrée II	146
752	7	Gigue	146
770	1	Allegro moderato	150
771	2	Adagio	150
772	3	Presto	150
782	1	Allegro aperto	154
783	2	Adagio	154
784	3	Rondeau. Tempo di Menuetto	154
790	1	Allegro	158
791	2	Larghetto	158
792	3	Allegro	158
800	1	Allegro	162
801	2	Andante	162
802	3	Allegro	162
811	0		166
\.


--
-- Name: composers_id_seq; Type: SEQUENCE SET; Schema: public; Owner: joseph
--

SELECT pg_catalog.setval('public.composers_id_seq', 24, true);


--
-- Name: movements_id_seq; Type: SEQUENCE SET; Schema: public; Owner: joseph
--

SELECT pg_catalog.setval('public.movements_id_seq', 820, true);


--
-- Name: works_id_seq; Type: SEQUENCE SET; Schema: public; Owner: joseph
--

SELECT pg_catalog.setval('public.works_id_seq', 171, true);


--
-- PostgreSQL database dump complete
--

