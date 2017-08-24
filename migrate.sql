ALTER TABLE "compose" ADD COLUMN IF NOT EXISTS "release" VARCHAR;
ALTER TABLE "compose" ADD COLUMN IF NOT EXISTS "version" VARCHAR;
ALTER TABLE "compose" ADD COLUMN IF NOT EXISTS "date" VARCHAR;
ALTER TABLE "compose" ADD COLUMN IF NOT EXISTS "respin" INT8;

UPDATE "compose" SET "release"=rtrim("compose_id", '0123456789-.');
UPDATE "compose" SET "version"=reverse(split_part(reverse("compose_id"), '-', 2));
UPDATE "compose" SET "date"=split_part(reverse(split_part(reverse("compose_id"), '-', 1)), '.', 1);
UPDATE "compose" SET "respin"=CAST(reverse(split_part(reverse("compose_id"), '.', 1)) AS INT8);

ALTER TABLE "compose" ALTER COLUMN "release" SET NOT NULL;
ALTER TABLE "compose" ALTER COLUMN "version" SET NOT NULL;
ALTER TABLE "compose" ALTER COLUMN "date" SET NOT NULL;
ALTER TABLE "compose" ALTER COLUMN "respin" SET NOT NULL;
