--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.5
-- Dumped by pg_dump version 9.5.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET search_path = public, pg_catalog;

--
-- Data for Name: compose; Type: TABLE DATA; Schema: public; Owner: talkcompose
--

INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (3, 'Fedora-Docker-25-20170726.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Docker-25-20170726.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (4, 'Fedora-Cloud-24-20170726.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-24-20170726.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (1, 'Fedora-Atomic-25-20170726.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-25-20170726.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (6, 'Fedora-Docker-25-20170724.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Docker-25-20170724.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (7, 'Fedora-Cloud-24-20170724.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-24-20170724.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (8, 'Fedora-Cloud-25-20170724.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-25-20170724.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (9, 'Fedora-Atomic-26-20170724.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170724.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (10, 'Fedora-Docker-25-20170725.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Docker-25-20170725.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (11, 'Fedora-Atomic-25-20170725.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-25-20170725.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (12, 'Fedora-Cloud-24-20170725.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-24-20170725.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (13, 'Fedora-Cloud-25-20170725.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-25-20170725.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (14, 'Fedora-Atomic-26-20170725.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170725.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (5, 'Fedora-Cloud-25-20170726.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-25-20170726.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (16, 'Fedora-Cloud-26-20170726.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-26-20170726.0/compose', 'DOOMED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (15, 'Fedora-Atomic-26-20170726.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170726.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (2, 'Fedora-Rawhide-20170726.n.0', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170726.n.0/compose', 'DOOMED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (17, 'Fedora-Rawhide-20170725.n.0', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170725.n.0/compose', 'DOOMED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (20, 'Fedora-Docker-25-20170727.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Docker-25-20170727.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (21, 'Fedora-Cloud-24-20170727.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-24-20170727.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (18, 'Fedora-Atomic-25-20170727.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-25-20170727.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (22, 'Fedora-Cloud-25-20170727.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-25-20170727.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (24, 'Fedora-Cloud-26-20170727.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-26-20170727.0/compose', 'DOOMED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (23, 'Fedora-Atomic-26-20170727.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170727.0/compose', 'FINISHED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (19, 'Fedora-Rawhide-20170727.n.0', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170727.n.0/compose', 'DOOMED', '2017-07-27 17:48:18.413883+00', '2017-07-27 17:48:18.413883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (25, 'Fedora-Atomic-26-20170728.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170728.0/compose', 'FINISHED', '2017-07-28 09:37:15.170987+00', '2017-07-28 09:37:15.170987+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (26, 'Fedora-Rawhide-20170728.n.0', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170728.n.0/compose', 'DOOMED', '2017-07-28 10:31:26.009128+00', '2017-07-28 10:31:26.009128+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (30, 'Fedora-Docker-25-20170729.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Docker-25-20170729.0/compose', 'FINISHED', '2017-07-29 05:45:18.043715+00', '2017-07-29 05:48:48.727573+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (32, 'Fedora-Cloud-24-20170729.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-24-20170729.0/compose', 'FINISHED', '2017-07-29 06:15:15.147111+00', '2017-07-29 06:26:00.571008+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (28, 'Fedora-Atomic-25-20170729.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-25-20170729.0/compose', 'FINISHED', '2017-07-29 05:15:18.822977+00', '2017-07-29 06:40:07.290879+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (36, 'Fedora-Cloud-26-20170729.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-26-20170729.0/compose', 'DOOMED', '2017-07-29 09:15:15.818572+00', '2017-07-29 09:16:51.697594+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (35, 'Fedora-Atomic-26-20170729.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170729.0/compose', 'FINISHED', '2017-07-29 08:15:16.575229+00', '2017-07-29 09:47:18.325934+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (27, 'Fedora-Rawhide-20170728.n.1', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170728.n.1/compose', 'DOOMED', '2017-07-28 18:01:54.917402+00', '2017-07-29 13:56:24.463039+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (29, 'Fedora-Rawhide-20170729.n.0', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170729.n.0/compose', 'DOOMED', '2017-07-29 05:16:54.520871+00', '2017-07-29 13:57:04.540636+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (44, 'Fedora-Docker-25-20170730.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Docker-25-20170730.0/compose', 'FINISHED', '2017-07-30 05:45:17.116946+00', '2017-07-30 05:48:22.807873+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (42, 'Fedora-Atomic-25-20170730.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-25-20170730.0/compose', 'FINISHED', '2017-07-30 05:15:19.513736+00', '2017-07-30 06:16:33.274679+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (46, 'Fedora-Cloud-24-20170730.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-24-20170730.0/compose', 'FINISHED', '2017-07-30 06:15:17.333369+00', '2017-07-30 06:27:25.351126+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (43, 'Fedora-Rawhide-20170730.n.0', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170730.n.0/compose', 'DOOMED', '2017-07-30 05:16:55.882664+00', '2017-07-30 06:30:18.902339+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (41, 'Fedora-Rawhide-20170729.n.1', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170729.n.1/compose', 'FINISHED_INCOMPLETE', '2017-07-29 17:30:53.726468+00', '2017-07-30 07:01:22.803036+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (51, 'Fedora-Cloud-25-20170730.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-25-20170730.0/compose', 'FINISHED', '2017-07-30 07:15:15.722058+00', '2017-07-30 07:24:17.181755+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (54, 'Fedora-Cloud-26-20170730.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-26-20170730.0/compose', 'DOOMED', '2017-07-30 09:15:15.545893+00', '2017-07-30 09:16:52.519448+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (53, 'Fedora-Atomic-26-20170730.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170730.0/compose', 'FINISHED', '2017-07-30 08:15:15.648511+00', '2017-07-30 09:44:29.080778+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (59, 'Fedora-Docker-25-20170731.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Docker-25-20170731.0/compose', 'FINISHED', '2017-07-31 05:45:18.812634+00', '2017-07-31 05:48:53.158915+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (61, 'Fedora-Cloud-24-20170731.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-24-20170731.0/compose', 'FINISHED', '2017-07-31 06:25:38.22606+00', '2017-07-31 06:25:38.22606+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (57, 'Fedora-Atomic-25-20170731.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-25-20170731.0/compose', 'FINISHED', '2017-07-31 05:15:23.392102+00', '2017-07-31 06:47:35.610883+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (58, 'Fedora-Rawhide-20170731.n.0', 'http://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20170731.n.0/compose', 'FINISHED_INCOMPLETE', '2017-07-31 05:16:56.954053+00', '2017-07-31 16:58:51.750244+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (63, 'Fedora-Cloud-25-20170731.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-25-20170731.0/compose', 'FINISHED', '2017-07-31 07:25:03.280201+00', '2017-07-31 07:25:03.280201+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (65, 'Fedora-Cloud-26-20170731.0', 'http://kojipkgs.fedoraproject.org/compose/Fedora-Cloud-26-20170731.0/compose', 'DOOMED', '2017-07-31 09:15:16.643708+00', '2017-07-31 09:17:11.848113+00');
INSERT INTO compose (id, compose_id, location, status, created_on, modified_on) VALUES (64, 'Fedora-Atomic-26-20170731.0', 'http://kojipkgs.fedoraproject.org/compose/twoweek/Fedora-Atomic-26-20170731.0/compose', 'FINISHED', '2017-07-31 08:15:16.275799+00', '2017-07-31 09:53:42.545308+00');


--
-- Name: compose_id_seq; Type: SEQUENCE SET; Schema: public; Owner: talkcompose
--

SELECT pg_catalog.setval('compose_id_seq', 68, true);


--
-- PostgreSQL database dump complete
--

