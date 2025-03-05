// date(-time) range processsor using git approxidate
// License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
// Disclaimer: Eric doesn't know C++
#include "approxidate.h"
#include <float.h>
#include <regex.h>

enum date_fmt {
	YYYYmmdd = 0,
	YYYYmmddHHMMSS,
	epoch_sec
};

static regex_t m_YYYYmmdd, m_YYYYmmddHHMMSS, m_epoch_sec;

static void xh_date_init(void)
{
	int e = regcomp(&m_YYYYmmdd,
			"^[0-9][0-9][0-9][0-9]" "[0-9][0-9]" "[0-9][0-9]$",
			REG_EXTENDED|REG_NOSUB);
	if (e) err(EXIT_FAILURE, "regcomp YYYYmmdd: %d", e);

	e = regcomp(&m_YYYYmmddHHMMSS,
			"^[0-9][0-9][0-9][0-9]" "[0-9][0-9]" "[0-9][0-9]"
			"[0-9][0-9]" "[0-9][0-9]" "[0-9][0-9]$",
			REG_EXTENDED|REG_NOSUB);
	if (e) err(EXIT_FAILURE, "regcomp YYYYmmddHHMMSS: %d", e);

	// let git interpret "YYYY", consider anything with more than
	// 5 digits to be an epoch date
	e = regcomp(&m_epoch_sec, "^[0-9][0-9][0-9][0-9][0-9]+$",
			REG_EXTENDED|REG_NOSUB);
	if (e) err(EXIT_FAILURE, "regcomp epoch_sec: %d", e);
}

static double to_column_fmt(enum date_fmt date_fmt, const std::string date)
{
	char buf[sizeof("YYYY-mm-dd HH:MM:SS")];
	char *end;
	time_t tmp;
	struct tm tm;
	long long v;
	git_time_t gt;
	const char *fmt = NULL;
	const char *c_str = date.c_str();

	// bypass git date parsing if already in expected formats
	switch (date_fmt) {
	case YYYYmmdd:
		if (!regexec(&m_YYYYmmdd, c_str, 0, NULL, 0))
			goto mkdouble;
		fmt = "%Y%m%d";
		break;
	case YYYYmmddHHMMSS:
		if (!regexec(&m_YYYYmmddHHMMSS, c_str, 0, NULL, 0))
			goto mkdouble;
		fmt = "%Y%m%d%H%M%S";
		break;
	case epoch_sec:
		if (!regexec(&m_epoch_sec, c_str, 0, NULL, 0))
			goto mkdouble;
	}
	if (git_date_parse(&gt, c_str))
		throw Xapian::QueryParserError("can't parse " + date);
	if (date_fmt == epoch_sec)
		return (double)gt;
	tmp = (time_t)gt;
	if ((git_time_t)tmp != gt)
		throw Xapian::QueryParserError("time out-of-range(system)");
	if (!gmtime_r(&tmp, &tm))
		throw Xapian::QueryParserError("gmtime_r failed");
	if (date_fmt == YYYYmmdd) // d: is low-precision
		tm.tm_sec = tm.tm_min = tm.tm_hour = 0;
	if (!strftime(buf, sizeof(buf), fmt, &tm))
		throw Xapian::QueryParserError("strftime failed");
	c_str = buf;
mkdouble:
	errno = 0;
	v = strtoll(c_str, &end, 10);
	if (*end || ((v == LLONG_MAX || v == LLONG_MIN) && errno == ERANGE))
		throw Xapian::QueryParserError("time out-of-range(?)");

	return (double)v;
}

class GitDateRangeProcessor : public Xapian::RangeProcessor {
protected:
	enum date_fmt date_fmt;
public:
	GitDateRangeProcessor(Xapian::valueno slot, const std::string prefix,
				enum date_fmt date_fmt_) :
		Xapian::RangeProcessor(slot, prefix, 0), date_fmt(date_fmt_) {}
	Xapian::Query operator()(const std::string &b, const std::string &e);
};

// Xapian calls this when processing queries
Xapian::Query GitDateRangeProcessor::operator()(const std::string &b,
						const std::string &e)
{
	double from = DBL_MIN, to = DBL_MAX;

	if (!b.empty())
		from = to_column_fmt(date_fmt, b);
	if (e.empty())
		return Xapian::Query(Xapian::Query::OP_VALUE_GE, slot,
					Xapian::sortable_serialise(from));
	to = to_column_fmt(date_fmt, e);
	if (b.empty())
		return Xapian::Query(Xapian::Query::OP_VALUE_LE, slot,
					Xapian::sortable_serialise(to));
	return Xapian::Query(Xapian::Query::OP_VALUE_RANGE, slot,
				Xapian::sortable_serialise(from),
				Xapian::sortable_serialise(to));
}

class GitDateFieldProcessor : public Xapian::FieldProcessor {
private:
	Xapian::valueno slot;
	enum date_fmt date_fmt;
public:
	GitDateFieldProcessor(Xapian::valueno slot_, enum date_fmt date_fmt_)
		: slot(slot_), date_fmt(date_fmt_) {};
	Xapian::Query operator()(const std::string &date);
};

// for dt:, rt:, d: w/o `..', called by Xapian after ->add_boolean_prefix
Xapian::Query GitDateFieldProcessor::operator()(const std::string &date)
{
	double from = to_column_fmt(date_fmt, date);
	double to = from + 86400;

	return Xapian::Query(Xapian::Query::OP_VALUE_RANGE, slot,
				Xapian::sortable_serialise(from),
				Xapian::sortable_serialise(to));
}
