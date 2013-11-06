ci = require("ci")
dist = require("dist")

ci.init()
dist.init()

ci.prependenv("PATH", "/usr/opt/godi/bin")
ci.prependenv("PATH", "/usr/opt/godi/sbin")
ci.putenv("OUNIT_OUTPUT_HTML_DIR", dist.make_filename("ounit-log-$(suite_name).html"))
ci.putenv("OUNIT_OUTPUT_JUNIT_FILE", dist.make_filename("junit-$(suite_name).xml"))
ci.putenv("OUNIT_OUTPUT_FILE", dist.make_filename("ounit-log-$(suite_name)-$(shard_id).txt"))

ci.exec("make", "clean")
ci.exec("make", "all")
ci.exec_opts({may_fail=true}, "make", "test")

-- TODO:
-- Create documentation package.
-- ci.exec("make", "doc-dev-dist")
