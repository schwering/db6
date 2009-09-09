#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef ulong;
struct run {
	unsigned long count;
	unsigned long ins_ps_cpu;
	unsigned long sea_ps_cpu;
	unsigned long del_ps_cpu;
	unsigned long ant_ps_cpu;
	unsigned long rei_ps_cpu;
	unsigned long ins_ps_rel;
	unsigned long sea_ps_rel;
	unsigned long del_ps_rel;
	unsigned long ant_ps_rel;
	unsigned long rei_ps_rel;
	double cpu_user;
	double cpu_nice;
	double cpu_system;
	double cpu_iowait;
	double cpu_steal;
	double cpu_idle;
	double dev_rs;
	double dev_ws;
	double dev_rkbs;
	double dev_wkbs;
	double dev_await;
	double dev_util;
};

#define CHECK(j) if (i==j){*last = 0; return;}

void scan(const char *buf, struct run *run, bool *last)
{
	int i;
	
	i=sscanf(buf,"Size = %lu", &run->count);
	CHECK(1);
	i=sscanf(buf,"INSERTING %*lu %*lu: %*um %*us (%lu) %*um %*us (%lu)",
			&run->ins_ps_cpu, &run->ins_ps_rel);
	CHECK(2);
	i=sscanf(buf,"SEARCH %*lu %*lu: %*um %*us (%lu) %*um %*us (%lu)",
			&run->sea_ps_cpu, &run->sea_ps_rel);
	CHECK(2);
	i=sscanf(buf,"DELETE %*lu %*lu: %*um %*us (%lu) %*um %*us (%lu)",
			&run->del_ps_cpu, &run->del_ps_rel);
	CHECK(2);
	i=sscanf(buf,"ANTISEARCH %*lu %*lu: %*um %*us (%lu) %*um %*us (%lu)",
			&run->ant_ps_cpu, &run->ant_ps_rel);
	CHECK(2);
	i=sscanf(buf,"REINSERTING %*lu %*lu: %*um %*us (%lu) %*um %*us (%lu)",
			&run->rei_ps_cpu, &run->rei_ps_rel);
	CHECK(2);
	i=sscanf(buf,"%lf %lf %lf %lf %lf %lf",
			&run->cpu_user, &run->cpu_nice, &run->cpu_system,
			&run->cpu_iowait, &run->cpu_steal, &run->cpu_idle);
	CHECK(6);
	i=sscanf(buf,"sda%*d %*lf %*lf %lf %lf %lf %lf %*lf %*lf %lf %*lf %lf",
			&run->dev_rs, &run->dev_ws, &run->dev_rkbs,
			&run->dev_wkbs, &run->dev_await, &run->dev_util); 
	if (i == 6) {
		*last = 1;
		return;
	}
	*last = 0;
}

#define PH(n) printf("%15s\t", #n)

void print_header(void)
{
	PH(count);
	PH(ins_ps_rel);
	PH(sea_ps_rel);
	//PH(del_ps_rel);
	//PH(ant_ps_rel);
	//PH(rei_ps_rel);
	PH(ins_ps_cpu);
	PH(sea_ps_cpu);
	//PH(del_ps_cpu);
	//PH(ant_ps_cpu);
	//PH(rei_ps_cpu);
	PH(cpu_user);
	PH(cpu_nice);
	PH(cpu_system);
	PH(cpu_iowait);
	PH(cpu_steal);
	PH(cpu_idle);
	PH(dev_rs);
	PH(dev_ws);
	PH(dev_rkbs);
	PH(dev_wkbs);
	PH(dev_await);
	PH(dev_util);
	printf("\n");
}

static char buf[128];
static char *sep(unsigned long n)
{
	int i, len, off;
	static char tmp[128];
	sprintf(tmp, "%lu", n);
	len = strlen(tmp);
	off = 0;
	for (i = 0; i < len; i++) {
		if ((len-i) % 3 == 0 && i > 0) {
			buf[i+off] = ',';
			off++;
		}
		buf[i+off] = tmp[i];
	}
	buf[len+off] = '\0';
	return buf;
}

//#define PL(c) printf("%15s\t", sep(run->c))
#define PL(c) printf("%15lu\t", run->c)
#define PD(c) printf("%15.5lf\t", run->c)

void print(const struct run *run)
{
	PL(count);
	PL(ins_ps_rel);
	PL(sea_ps_rel);
	//PL(del_ps_rel);
	//PL(ant_ps_rel);
	//PL(rei_ps_rel);
	PL(ins_ps_cpu);
	PL(sea_ps_cpu);
	//PL(del_ps_cpu);
	//PL(ant_ps_cpu);
	//PL(rei_ps_cpu);
	PD(cpu_user);
	PD(cpu_nice);
	PD(cpu_system);
	PD(cpu_iowait);
	PD(cpu_steal);
	PD(cpu_idle);
	PD(dev_rs);
	PD(dev_ws);
	PD(dev_rkbs);
	PD(dev_wkbs);
	PD(dev_await);
	PD(dev_util);
	printf("\n");
}

int main(int argc, char **argv)
{
	char buf[128];
	struct run run;
	FILE *fp;
	
	fp = fopen(argv[1], "r");
	if (!fp) {
		printf("geht nich\n");
		return 1;
	}
	print_header();
	while (fscanf(fp, "%128[^\n]\n", buf) == 1) {
		bool last;
		scan(buf, &run, &last);
		if (last) {
			print(&run);
			memset(&run, 0, sizeof(struct run));
		}
	}
	fclose(fp);
	return 0;
}

