package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"itn_json_types"
	"math"
	"math/rand"
	"os"
	"sort"
	"strconv"
	"strings"

	lib "itn_orchestrator"
)

/*
 * Returns random number in normal distribution centering on 0.
 * ~95% of numbers returned should fall between -2 and 2
 * ie within two standard deviations
 */
func gaussRandom() float64 {
	u := 2*rand.Float64() - 1
	v := 2*rand.Float64() - 1
	r := u*u + v*v
	// if outside interval [0,1] start over
	if r == 0 || r >= 1 {
		return gaussRandom()
	}

	c := math.Sqrt(-2 * math.Log(r) / r)
	return u * c
}

func sampleTps(baseTps, stressTps float64) float64 {
	tpsStddev := (stressTps - baseTps) / 2
	return tpsStddev*math.Abs(gaussRandom()) + baseTps
}

func sampleStopRatio(minRatio, maxRatio float64) float64 {
	stddev := (maxRatio - minRatio) / 3
	return stddev*math.Abs(gaussRandom()) + minRatio
}

type Params struct {
	MinTps, BaseTps, StressTps, SenderRatio, ZkappRatio, NewAccountRatio float64
	StopCleanRatio, MinStopRatio, MaxStopRatio                           float64
	RoundDurationMin, PauseMin, Rounds, StopsPerRound, Gap               int
	SendFromNonBpsOnly, StopOnlyBps, UseRestartScript, MaxCost           bool
	ExperimentName, PasswordEnv, FundKeyPrefix                           string
	Privkeys                                                             []string
	PaymentReceiver                                                      itn_json_types.MinaPublicKey
	PrivkeysPerFundCmd                                                   int
	GenerateFundKeys                                                     int
	RotationKeys, RotationServers                                        []string
	RotationPermutation                                                  bool
	RotationRatio                                                        float64
	MixMaxCostTpsRatio                                                   float64
	LargePauseEveryNRounds, LargePauseMin                                int
	MinBalanceChange, MaxBalanceChange, DeploymentFee                    uint64
	PaymentAmount, MinFee, MaxFee, FundFee                               uint64
}

type Command struct {
	Action  string `json:"action"`
	Params  any    `json:"params"`
	comment string
}

type GeneratedRound struct {
	Commands     []Command
	FundCommands []lib.FundParams
}

func fund(p lib.FundParams) Command {
	return Command{Action: lib.FundAction{}.Name(), Params: p}
}

func rotate(p lib.RotateParams) Command {
	return Command{Action: lib.RotateAction{}.Name(), Params: p}
}

func loadKeys(p lib.KeyloaderParams) Command {
	return Command{Action: lib.KeyloaderAction{}.Name(), Params: p}
}

func discovery(p lib.DiscoveryParams) Command {
	return Command{Action: lib.DiscoveryAction{}.Name(), Params: p}
}

type SampleRefParams struct {
	Group  lib.ComplexValue `json:"group"`
	Ratios []float64        `json:"ratios"`
}

func sample(groupRef int, groupName string, ratios []float64) Command {
	group := lib.LocalComplexValue(groupRef, groupName)
	group.OnEmpty = emptyArrayRawMessage
	return Command{Action: lib.SampleAction{}.Name(), Params: SampleRefParams{
		Group:  group,
		Ratios: ratios,
	}}
}

type ZkappRefParams struct {
	lib.ZkappSubParams
	FeePayers lib.ComplexValue `json:"feePayers"`
	Nodes     lib.ComplexValue `json:"nodes"`
}

func zkapps(feePayersRef int, nodesRef int, nodesName string, params lib.ZkappSubParams) Command {
	cmd := Command{Action: lib.ZkappCommandsAction{}.Name(), Params: ZkappRefParams{
		ZkappSubParams: params,
		FeePayers:      lib.LocalComplexValue(feePayersRef, "key"),
		Nodes:          lib.LocalComplexValue(nodesRef, nodesName),
	}}
	maxCostStr := ""
	if params.MaxCost {
		maxCostStr = "max-cost "
	}
	comment := fmt.Sprintf("Scheduling %d %szkapp transactions to be sent over period of %d minutes (%.2f txs/min)",
		int(params.Tps*float64(params.DurationMin)*60), maxCostStr, params.DurationMin, params.Tps*60,
	)
	return withComment(comment, cmd)
}

type PaymentRefParams struct {
	lib.PaymentSubParams
	FeePayers lib.ComplexValue `json:"feePayers"`
	Nodes     lib.ComplexValue `json:"nodes"`
}

func payments(feePayersRef int, nodesRef int, nodesName string, params lib.PaymentSubParams) Command {
	cmd := Command{Action: lib.PaymentsAction{}.Name(), Params: PaymentRefParams{
		PaymentSubParams: params,
		FeePayers:        lib.LocalComplexValue(feePayersRef, "key"),
		Nodes:            lib.LocalComplexValue(nodesRef, nodesName),
	}}
	comment := fmt.Sprintf("Scheduling %d payments to be sent over period of %d minutes (%.2f txs/min)",
		int(params.Tps*float64(params.DurationMin)*60), params.DurationMin, params.Tps*60,
	)
	return withComment(comment, cmd)
}

func waitMin(min int) Command {
	return Command{Action: lib.WaitAction{}.Name(), Params: lib.WaitParams{
		Minutes: min,
	}}
}

func wait(sec int) Command {
	return Command{Action: lib.WaitAction{}.Name(), Params: lib.WaitParams{
		Seconds: sec,
	}}
}

type RestartRefParams struct {
	Nodes lib.ComplexValue `json:"nodes"`
	Clean bool             `json:"clean,omitempty"`
}

func stopDaemon(useRestartScript bool, nodesRef int, nodesName string, clean bool) Command {
	var name string
	if useRestartScript {
		name = lib.RestartAction{}.Name()
	} else {
		name = lib.StopDaemonAction{}.Name()
	}
	return Command{Action: name, Params: RestartRefParams{
		Nodes: lib.LocalComplexValue(nodesRef, nodesName),
		Clean: clean,
	}}
}

type JoinRefParams struct {
	Group1 lib.ComplexValue `json:"group1"`
	Group2 lib.ComplexValue `json:"group2"`
}

func join(g1Ref int, g1Name string, g2Ref int, g2Name string) Command {
	return Command{Action: lib.JoinAction{}.Name(), Params: JoinRefParams{
		Group1: lib.LocalComplexValue(g1Ref, g1Name),
		Group2: lib.LocalComplexValue(g2Ref, g2Name),
	}}
}

type ExceptRefParams struct {
	Group  lib.ComplexValue `json:"group"`
	Except lib.ComplexValue `json:"except"`
}

var emptyArrayRawMessage json.RawMessage

func init() {
	emptyArrayRawMessage, _ = json.Marshal([]string{})
}

func except(groupRef int, groupName string, exceptRef int, exceptName string) Command {
	group := lib.LocalComplexValue(groupRef, groupName)
	group.OnEmpty = emptyArrayRawMessage
	except := lib.LocalComplexValue(exceptRef, exceptName)
	except.OnEmpty = emptyArrayRawMessage
	return Command{Action: lib.ExceptAction{}.Name(), Params: ExceptRefParams{
		Group:  group,
		Except: except,
	}}
}

func withComment(comment string, cmd Command) Command {
	cmd.comment = comment
	return cmd
}

func formatDur(min, sec int) string {
	sec += min * 60
	min = sec / 60
	sec %= 60
	hour := min / 60
	min %= 60
	day := hour / 24
	hour %= 24
	parts := []string{}
	if day > 0 {
		parts = append(parts, strconv.Itoa(day), "days")
	}
	if hour > 0 {
		parts = append(parts, strconv.Itoa(hour), "hours")
	}
	if min > 0 {
		parts = append(parts, strconv.Itoa(min), "mins")
	}
	if sec > 0 {
		parts = append(parts, strconv.Itoa(sec), "secs")
	}
	if len(parts) == 0 {
		return "immediately"
	}
	return strings.Join(parts, " ")
}

func (p *Params) Generate(round int) GeneratedRound {
	zkappsKeysDir := fmt.Sprintf("%s/round-%d/zkapps", p.FundKeyPrefix, round)
	paymentsKeysDir := fmt.Sprintf("%s/round-%d/payments", p.FundKeyPrefix, round)
	tps := sampleTps(p.BaseTps, p.StressTps)
	maxCost := p.MaxCost
	zkappRatio := p.ZkappRatio
	if p.MixMaxCostTpsRatio > 1e-3 && (round&1) == 1 {
		maxCost = true
		zkappRatio = 1
		tps *= p.MixMaxCostTpsRatio
	}
	experimentName := fmt.Sprintf("%s-%d", p.ExperimentName, round)
	onlyZkapps := math.Abs(1-zkappRatio) < 1e-3
	onlyPayments := zkappRatio < 1e-3
	zkappTps := tps * zkappRatio
	zkappParams := lib.ZkappSubParams{
		ExperimentName:   experimentName,
		Tps:              zkappTps,
		MinTps:           p.MinTps,
		DurationMin:      p.RoundDurationMin,
		Gap:              p.Gap,
		MinBalanceChange: p.MinBalanceChange,
		MaxBalanceChange: p.MaxBalanceChange,
		MinFee:           p.MinFee,
		MaxFee:           p.MaxFee,
		DeploymentFee:    p.DeploymentFee,
		MaxCost:          maxCost,
		NewAccountRatio:  p.NewAccountRatio,
	}
	if maxCost {
		// This can be set to arbitrary value as for max-cost it only
		// matters that total zkapps deployed is above 5
		// We need to set it this way to override setting accountQueueSize
		// by the orchestrator
		zkappParams.ZkappsToDeploy = 20
		zkappParams.NewAccountRatio = 0
	}
	paymentParams := lib.PaymentSubParams{
		ExperimentName: experimentName,
		Tps:            tps - zkappTps,
		MinTps:         p.MinTps,
		DurationMin:    p.RoundDurationMin,
		MinFee:         p.MinFee,
		MaxFee:         p.MaxFee,
		Amount:         p.PaymentAmount,
		Receiver:       p.PaymentReceiver,
	}
	cmds := []Command{}
	roundStartMin := round*(p.RoundDurationMin+p.PauseMin) + round/p.LargePauseEveryNRounds*p.LargePauseMin
	if len(p.RotationKeys) > 0 {
		var mapping []int
		nKeys := len(p.RotationKeys)
		if p.RotationPermutation {
			mapping = rand.Perm(nKeys)
		} else {
			mapping = make([]int, nKeys)
			for i := range mapping {
				mapping[i] = rand.Intn(len(p.RotationKeys))
			}
		}
		cmds = append(cmds, rotate(lib.RotateParams{
			Pubkeys:     p.RotationKeys,
			RestServers: p.RotationServers,
			Mapping:     mapping,
			Ratio:       p.RotationRatio,
			PasswordEnv: p.PasswordEnv,
		}))
	}
	cmds = append(cmds, withComment(fmt.Sprintf("Starting round %d, %s after start", round, formatDur(roundStartMin, 0)), discovery(lib.DiscoveryParams{
		OffsetMin:        15,
		NoBlockProducers: p.SendFromNonBpsOnly,
	})))
	sendersOutName := "participant"
	if 1-p.SenderRatio > 1e-6 {
		sendersOutName = "group1"
		cmds = append(cmds, sample(-1, "participant", []float64{p.SenderRatio}))
	}
	if onlyPayments {
		cmds = append(cmds, loadKeys(lib.KeyloaderParams{Dir: paymentsKeysDir}))
		cmds = append(cmds, payments(-1, -2, sendersOutName, paymentParams))
	} else if onlyZkapps {
		cmds = append(cmds, loadKeys(lib.KeyloaderParams{Dir: zkappsKeysDir}))
		cmds = append(cmds, zkapps(-1, -2, sendersOutName, zkappParams))
	} else {
		cmds = append(cmds, loadKeys(lib.KeyloaderParams{Dir: zkappsKeysDir}))
		cmds = append(cmds, loadKeys(lib.KeyloaderParams{Dir: paymentsKeysDir}))
		cmds = append(cmds, zkapps(-2, -3, sendersOutName, zkappParams))
		cmds = append(cmds, payments(-2, -4, sendersOutName, paymentParams))
		cmds = append(cmds, join(-1, "participant", -2, "participant"))
	}
	sendersCmdId := len(cmds)
	stopWaits := make([]int, p.StopsPerRound)
	for i := 0; i < p.StopsPerRound; i++ {
		stopWaits[i] = rand.Intn(60 * p.RoundDurationMin)
	}
	sort.Ints(stopWaits)
	for i := p.StopsPerRound - 1; i > 0; i-- {
		stopWaits[i] -= stopWaits[i-1]
	}
	stopRatio := sampleStopRatio(p.MinStopRatio, p.MaxStopRatio)
	elapsed := 0
	for _, waitSec := range stopWaits {
		cmds = append(cmds, withComment(fmt.Sprintf("Running round %d, %s after start, waiting for %s", round, formatDur(roundStartMin, elapsed), formatDur(0, waitSec)), wait(waitSec)))
		cmds = append(cmds, discovery(lib.DiscoveryParams{
			OffsetMin:          15,
			OnlyBlockProducers: p.StopOnlyBps,
		}))
		exceptRefName := "group"
		if onlyPayments || onlyZkapps {
			exceptRefName = "participant"
		}
		cmds = append(cmds, except(-1, "participant", sendersCmdId-len(cmds)-1, exceptRefName))
		stopCleanRatio := p.StopCleanRatio * stopRatio
		stopNoCleanRatio := (1 - p.StopCleanRatio) * stopRatio
		nodesOrBps := "nodes"
		if p.StopOnlyBps {
			nodesOrBps = "block producers"
		}
		if stopCleanRatio > 1e-6 && stopNoCleanRatio > 1e-6 {
			cmds = append(cmds, sample(-1, "group", []float64{stopCleanRatio, stopNoCleanRatio}))
			comment1 := fmt.Sprintf("Stopping %.1f%% %s with cleaning", stopCleanRatio*100, nodesOrBps)
			cmds = append(cmds, withComment(comment1, stopDaemon(p.UseRestartScript, -1, "group1", true)))
			comment2 := fmt.Sprintf("Stopping %.1f%% %s without cleaning", stopNoCleanRatio*100, nodesOrBps)
			cmds = append(cmds, withComment(comment2, stopDaemon(p.UseRestartScript, -2, "group2", false)))
		} else if stopCleanRatio > 1e-6 {
			comment := fmt.Sprintf("Stopping %.1f%% %s with cleaning", stopCleanRatio*100, nodesOrBps)
			cmds = append(cmds, sample(-1, "group", []float64{stopCleanRatio}))
			cmds = append(cmds, withComment(comment, stopDaemon(p.UseRestartScript, -1, "group1", true)))
		} else if stopNoCleanRatio > 1e-6 {
			comment := fmt.Sprintf("Stopping %.1f%% %s without cleaning", stopNoCleanRatio*100, nodesOrBps)
			cmds = append(cmds, sample(-1, "group", []float64{stopNoCleanRatio}))
			cmds = append(cmds, withComment(comment, stopDaemon(p.UseRestartScript, -1, "group1", false)))
		}
		elapsed += waitSec
	}
	if round < p.Rounds-1 {
		comment1 := fmt.Sprintf("Waiting for remainder of round %d, %s after start", round, formatDur(roundStartMin, elapsed))
		cmds = append(cmds, withComment(comment1, wait(p.RoundDurationMin*60-elapsed)))
		if p.PauseMin > 0 {
			comment2 := fmt.Sprintf("Pause after round %d, %s after start", round, formatDur(roundStartMin+p.RoundDurationMin, 0))
			cmds = append(cmds, withComment(comment2, waitMin(p.PauseMin)))
		}
		if p.LargePauseMin > 0 && (round+1)%p.LargePauseEveryNRounds == 0 {
			comment3 := fmt.Sprintf("Large pause after round %d, %s after start", round, formatDur(roundStartMin+p.RoundDurationMin+p.PauseMin, 0))
			cmds = append(cmds, withComment(comment3, waitMin(p.LargePauseMin)))
		}
	}
	fundCmds := []lib.FundParams{}
	if !onlyPayments {
		_, _, _, initBalance := lib.ZkappBalanceRequirements(zkappTps, zkappParams)
		zkappKeysNum, zkappAmount := lib.ZkappKeygenRequirements(initBalance, zkappParams)
		fundCmds = append(fundCmds,
			lib.FundParams{
				PasswordEnv: p.PasswordEnv,
				Prefix:      zkappsKeysDir + "/key",
				Amount:      zkappAmount,
				Fee:         p.FundFee,
				Num:         zkappKeysNum,
			})
	}
	if !onlyZkapps {
		paymentKeysNum, paymentAmount := lib.PaymentKeygenRequirements(p.Gap, paymentParams)
		fundCmds = append(fundCmds,
			lib.FundParams{
				PasswordEnv: p.PasswordEnv,
				// Privkeys:    privkeys,
				Prefix: paymentsKeysDir + "/key",
				Amount: paymentAmount,
				Fee:    p.FundFee,
				Num:    paymentKeysNum,
			})
	}
	return GeneratedRound{
		Commands:     cmds,
		FundCommands: fundCmds,
	}
}

func checkRatio(ratio float64, msg string) {
	if ratio < 0.0 || ratio > 1.0 {
		fmt.Fprintln(os.Stderr, msg)
		os.Exit(2)
	}
}

const mixMaxCostTpsRatioHelp = "when provided, specifies ratio of tps (proportional to total tps) for max cost transactions to be used every other round, zkapps ratio for these rounds is set to 100%"

func main() {
	var rotateKeys, rotateServers string
	var mode string
	var p Params
	flag.Float64Var(&p.BaseTps, "base-tps", 0.3, "Base tps rate for the whole network")
	flag.Float64Var(&p.StressTps, "stress-tps", 1, "stress tps rate for the whole network")
	flag.Float64Var(&p.MinTps, "min-tps", 0.01, "minimal tps per node")
	flag.Float64Var(&p.MinStopRatio, "stop-min-ratio", 0.0, "float in range [0..1], minimum ratio of nodes to stop at an interval")
	flag.Float64Var(&p.MaxStopRatio, "stop-max-ratio", 0.5, "float in range [0..1], maximum ratio of nodes to stop at an interval")
	flag.Float64Var(&p.SenderRatio, "sender-ratio", 0.5, "float in range [0..1], max proportion of nodes selected for transaction sending")
	flag.Float64Var(&p.ZkappRatio, "zkapp-ratio", 0.5, "float in range [0..1], ratio of zkapp transactions of all transactions generated")
	flag.Float64Var(&p.StopCleanRatio, "stop-clean-ratio", 0.1, "float in range [0..1], ratio of stops with cleaning of all stops")
	flag.Float64Var(&p.NewAccountRatio, "new-account-ratio", 0, "float in range [0..1], ratio of new accounts, in relation to expected number of zkapp txs, ignored for max-cost txs")
	flag.BoolVar(&p.SendFromNonBpsOnly, "send-from-non-bps", false, "send only from non block producers")
	flag.BoolVar(&p.StopOnlyBps, "stop-only-bps", false, "stop only block producers")
	flag.BoolVar(&p.UseRestartScript, "use-restart-script", false, "use restart script insteadt of stop-daemon command")
	flag.BoolVar(&p.MaxCost, "max-cost", false, "send max-cost zkapp commands")
	flag.IntVar(&p.RoundDurationMin, "round-duration", 30, "duration of a round, minutes")
	flag.IntVar(&p.PauseMin, "pause", 15, "duration of a pause between rounds, minutes")
	flag.IntVar(&p.Rounds, "rounds", 4, "number of rounds to run experiment")
	flag.IntVar(&p.StopsPerRound, "round-stops", 2, "number of stops to perform within round")
	flag.IntVar(&p.Gap, "gap", 180, "gap between related transactions, seconds")
	flag.StringVar(&mode, "mode", "default", "mode of generation")
	flag.StringVar(&p.FundKeyPrefix, "fund-keys-dir", "./fund-keys", "Dir for generated fund key prefixes")
	flag.StringVar(&p.PasswordEnv, "password-env", "", "Name of environment variable to read privkey password from")
	flag.StringVar((*string)(&p.PaymentReceiver), "payment-receiver", "", "Mina PK receiving payments")
	flag.StringVar(&p.ExperimentName, "experiment-name", "exp-0", "Name of experiment")
	flag.IntVar(&p.PrivkeysPerFundCmd, "privkeys-per-fund", 1, "Number of private keys to use per fund command")
	flag.IntVar(&p.GenerateFundKeys, "generate-privkeys", 0, "Number of funding keys to generate from the private key")
	flag.StringVar(&rotateKeys, "rotate-keys", "", "Comma-separated list of public keys to rotate")
	flag.StringVar(&rotateServers, "rotate-servers", "", "Comma-separated list of servers for rotation")
	flag.Float64Var(&p.RotationRatio, "rotate-ratio", 0.3, "Ratio of balance to rotate")
	flag.BoolVar(&p.RotationPermutation, "rotate-permutation", false, "Whether to generate only permutation mappings for rotation")
	flag.IntVar(&p.LargePauseMin, "large-pause", 0, "duration of the large pause, minutes")
	flag.IntVar(&p.LargePauseEveryNRounds, "large-pause-every", 8, "number of rounds in between large pauses")
	flag.Float64Var(&p.MixMaxCostTpsRatio, "max-cost-mixed", 0, mixMaxCostTpsRatioHelp)
	flag.Uint64Var(&p.MaxBalanceChange, "max-balance-change", 1e3, "Max balance change for zkapp account update")
	flag.Uint64Var(&p.MinBalanceChange, "min-balance-change", 0, "Min balance change for zkapp account update")
	flag.Uint64Var(&p.DeploymentFee, "deployment-fee", 1e9, "Zkapp deployment fee")
	flag.Uint64Var(&p.FundFee, "fund-fee", 1e9, "Funding tx fee")
	flag.Uint64Var(&p.MinFee, "min-fee", 1e9, "Min tx fee")
	flag.Uint64Var(&p.MaxFee, "max-fee", 2e9, "Max tx fee")
	flag.Uint64Var(&p.PaymentAmount, "payment-amount", 1e5, "Payment amount")
	flag.Parse()
	checkRatio(p.SenderRatio, "wrong sender ratio")
	checkRatio(p.ZkappRatio, "wrong zkapp ratio")
	checkRatio(p.MinStopRatio, "wrong min stop ratio")
	checkRatio(p.MaxStopRatio, "wrong max stop ratio")
	checkRatio(p.StopCleanRatio, "wrong stop-clean ratio")
	checkRatio(p.MixMaxCostTpsRatio, "wrong max-cost-mixed ratio")
	if p.MaxCost && p.MixMaxCostTpsRatio > 1e-3 {
		fmt.Fprintln(os.Stderr, "both max-cost-mixed and max-cost specified")
		os.Exit(2)
	}
	if p.LargePauseEveryNRounds <= 0 {
		fmt.Fprintln(os.Stderr, "wrong large-pause-every: should be a positive number")
		os.Exit(2)
	}
	if p.RoundDurationMin*60 < p.Gap*4 {
		fmt.Fprintln(os.Stderr, "increase round duration: roundDurationMin*60 should be more than gap*4")
		os.Exit(9)
	}
	if p.NewAccountRatio < 0 {
		fmt.Fprintln(os.Stderr, "wrong new account ratio")
		os.Exit(2)
	}
	checkRatio(p.RotationRatio, "wrong rotation ratio")
	p.Privkeys = flag.Args()
	if len(p.Privkeys) == 0 {
		fmt.Fprintln(os.Stderr, "Specify funding private key files after all flags (separated by spaces)")
		os.Exit(4)
	}
	if p.GenerateFundKeys > 0 && len(p.Privkeys) > 1 {
		fmt.Fprintln(os.Stderr, "When option -generate-funding-keys is used, only a single private key should be provided")
		os.Exit(4)
	}
	if (p.GenerateFundKeys > 0 && p.GenerateFundKeys < p.PrivkeysPerFundCmd) || (p.GenerateFundKeys == 0 && len(p.Privkeys) < p.PrivkeysPerFundCmd) {
		fmt.Fprintln(os.Stderr, "Number of private keys is less than -privkeys-per-fund")
		os.Exit(4)
	}
	if rotateKeys != "" {
		p.RotationKeys = strings.Split(rotateKeys, ",")
	}
	if rotateServers != "" {
		p.RotationServers = strings.Split(rotateServers, ",")
	}
	if len(p.RotationServers) != len(p.RotationKeys) {
		fmt.Fprintln(os.Stderr, "wrong rotation configuration")
		os.Exit(5)
	}
	switch mode {
	case "stop-ratio-distribution":
		for i := 0; i < 10000; i++ {
			v := sampleStopRatio(p.MinStopRatio, p.MaxStopRatio)
			fmt.Println(v)
		}
		return
	case "tps-distribution":
		for i := 0; i < 10000; i++ {
			v := sampleTps(p.BaseTps, p.StressTps)
			fmt.Println(v)
		}
		return
	case "default":
	default:
		os.Exit(1)
	}
	if p.PaymentReceiver == "" && p.ZkappRatio < 0.999 {
		fmt.Fprintln(os.Stderr, "Payment receiver not specified")
		os.Exit(2)
	}
	encoder := json.NewEncoder(os.Stdout)
	writeComment := func(comment string) {
		if err := encoder.Encode(comment); err != nil {
			fmt.Fprintf(os.Stderr, "Error writing comment: %v\n", err)
			os.Exit(3)
		}
	}
	writeComment("Generated with: " + strings.Join(os.Args, " "))
	writeComment("Funding keys for the experiment")
	writeCommand := func(cmd Command) {
		if cmd.comment != "" {
			writeComment(cmd.comment)
		}
		if err := encoder.Encode(cmd); err != nil {
			fmt.Fprintf(os.Stderr, "Error writing command: %v\n", err)
			os.Exit(3)
		}
	}
	cmds := []Command{}
	fundCmds := []lib.FundParams{}
	for r := 0; r < p.Rounds; r++ {
		round := p.Generate(r)
		cmds = append(cmds, round.Commands...)
		fundCmds = append(fundCmds, round.FundCommands...)
	}
	privkeys := p.Privkeys
	if p.GenerateFundKeys > 0 {
		fundKeysDir := fmt.Sprintf("%s/funding", p.FundKeyPrefix)
		privkeys = make([]string, p.GenerateFundKeys)
		privkeyAmounts := make([]uint64, p.GenerateFundKeys)
		for i := range privkeys {
			privkeys[i] = fmt.Sprintf("%s/key-0-%d", fundKeysDir, i)
		}
		for i, f := range fundCmds {
			i_ := (i * p.PrivkeysPerFundCmd) % p.GenerateFundKeys
			itemsPerFundKey := f.Num/p.PrivkeysPerFundCmd + 1
			perGeneratedKey := f.Amount / uint64(f.Num) * uint64(itemsPerFundKey)
			for j := i_; j < (i_ + p.PrivkeysPerFundCmd); j++ {
				j_ := j % p.GenerateFundKeys
				privkeyAmounts[j_] += perGeneratedKey
			}
		}
		perKeyAmount := privkeyAmounts[0]
		for _, a := range privkeyAmounts[1:] {
			if perKeyAmount < a {
				perKeyAmount = a
			}
		}
		// Generate funding keys
		writeCommand(fund(lib.FundParams{
			PasswordEnv: p.PasswordEnv,
			Privkeys:    p.Privkeys,
			Prefix:      fundKeysDir + "/key",
			Amount:      perKeyAmount*uint64(p.GenerateFundKeys)*3/2 + 2e9,
			Fee:         p.FundFee,
			Num:         p.GenerateFundKeys,
		}))
		writeCommand(wait(1))
	}
	privkeysExt := append(privkeys, privkeys...)
	for i, cmd := range fundCmds {
		i_ := (i * p.PrivkeysPerFundCmd) % len(privkeys)
		cmd.Privkeys = privkeysExt[i_:(i_ + p.PrivkeysPerFundCmd)]
		writeCommand(fund(cmd))
	}
	for _, cmd := range cmds {
		writeCommand(cmd)
	}
}
